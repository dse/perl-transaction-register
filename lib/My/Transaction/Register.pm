package My::Transaction::Register;
use warnings;
use strict;

require 5.10.1;
use feature "switch";

use POSIX qw(strftime);
use Carp qw(croak);
use List::MoreUtils qw(all);
use Text::Tabs qw(expand unexpand);

use constant LOOSELY_DEFINED_DATE => 1;
use constant LOOSELY_DEFINED_AMOUNT => 1;

# We implement these as variables so we can interpolate them into
# regexes.
use vars qw($RX_DATE $RX_AMOUNT $RX_LINE_1 $RX_LINE_2);
BEGIN {
    $RX_DATE =
      qr{\?\?\?\?-\?\?-\?\?|    # unknown dates
         \d\d\d\d-\?\?-\?\?|
         \d\d\d\d-\d\d-\?\?|
         \d\d/\?\?|
         \?\?/\?\?/\?\?\?\?|
         \?\?/\?\?/\d\d\d\d|
         \d\d/\?\?/\d\d\d\d|
         \d\d\d\d-\d\d-\d\d|	# known dates
         \d\d/\d\d|
         \d\d/\d\d/\d\d\d\d
         #}x;

    if (LOOSELY_DEFINED_DATE) {
        $RX_DATE = qr{\S+}x;
    }
    $RX_AMOUNT =
      qr{\$?
         \(\$?\d+\.\d\d\)|	# for checking: deposit/credit
         -?\$?-?\d+\.\d\d\b	# for checking: withdrawal/debit
         #}x;

    my $RX_PARSE_DATE   = $RX_DATE;
    my $RX_PARSE_AMOUNT = $RX_AMOUNT;

    if (LOOSELY_DEFINED_DATE) {
        $RX_PARSE_DATE = qr{\S+}xi;
    }
    if (LOOSELY_DEFINED_AMOUNT) {
        $RX_PARSE_AMOUNT = qr{(?:$RX_AMOUNT|\?+)}xi;
    }

    # optional flag, then date, then amount, then merchant
    $RX_LINE_1 =
      qr{^\s*
         (?:([\.\-\*\/\+\!]+)\s*)? # optional flag (posted, pending, future, etc.)
         ($RX_PARSE_DATE)\s+
         ($RX_PARSE_AMOUNT)\s+
         (\S.*?)		# merchant
         \s*$
         #}x;

    # optional flag, then date, then merchant, then amount
    $RX_LINE_2 =
      qr{^\s*
         (?:([\.\-\*\/\+\!]+)\s*)? # optional flag (posted, pending, future, etc.)
         ($RX_PARSE_DATE)\s+
         (\S.*?)                # merchant
         \s+
         ($RX_PARSE_AMOUNT)\s*$
         #}x;
}

use Moose;
has 'start_balance'  => (is => 'rw', isa => 'Num',               default => 0);
has 'entries'        => (is => 'rw', isa => 'ArrayRef[HashRef]', default => sub { return []; });
has 'lines'          => (is => 'rw', isa => 'ArrayRef[HashRef]', default => sub { return []; });
has 'checkpoints'    => (is => 'rw', isa => 'ArrayRef[HashRef]', default => sub { return []; });
has 'account_type'   => (is => 'rw', isa => 'Str',               default => "checking");
has 'start_date'     => (is => 'rw', isa => 'Str',               default => "1970-01-01");
has '_stop_parsing'  => (is => 'rw', isa => 'Bool',              default => 0);
has '_last_date'     => (is => 'rw', isa => 'Str|Undef');
has '_last_date_fmt' => (is => 'rw', isa => 'Str|Undef');
has 'comment_style'  => (is => 'rw', isa => 'Str',               default => 'c++');
has 'strip_comments' => (is => 'rw', isa => 'Bool',              default => 1);

sub multiplier {
    my ($self) = @_;
    my $type = $self->account_type;
    if ($type eq "checking") {
        return 1;
    }
    if ($type eq "credit-card" || $type eq "credit") {
        return -1;
    }
    return 1;
}

sub balance {
    my ($self, %args) = @_;
    my $filter     = $args{filter};
    my $map_amount = $args{map_amount};
    my $offset     = $args{offset} // 0;
    my $length     = $args{length} // (scalar(@{$self->entries}) - $offset);
    my $result     = $args{start}  // $self->start_balance;
    for (my $i = $offset; $i < ($offset + $length); $i += 1) {
        my $entry = $self->entries->[$i];
        if ($filter) {
            next if !($filter->($entry));
        }
        if ($map_amount) {
            $result += $self->multiplier * $map_amount->($entry);
        } else {
            $result += $self->multiplier * ($entry->amount // 0);
        }
    }
    return $result;
}

sub running_balance {
    my ($self, $start, $offset, $length) = @_;
    return $self->balance(
        start => $start,
        offset => $offset,
        length => $length,
        filter => sub {
            my ($entry) = @_;
            return !$entry->is_future && !$entry->is_todo;
        }
    );
}

sub pending_balance {
    my ($self, $start, $offset, $length) = @_;
    return $self->balance(
        start => $start,
        offset => $offset,
        length => $length,
        filter => sub {
            my ($entry) = @_;
            return ($entry->is_pending || $entry->is_posted) && !$entry->is_future && !$entry->is_todo;
        },
        map_amount => sub {
            my ($entry) = @_;
            if ($entry->is_pending) {
                return $entry->pending_amount // $entry->amount;
            } else {
                return $entry->amount;
            }
        }
    );
}

sub posted_balance {
    my ($self, $start, $offset, $length) = @_;
    return $self->balance(
        start => $start,
        offset => $offset,
        length => $length,
        filter => sub {
            my $entry = shift();
            return $entry->is_posted && !$entry->is_future && !$entry->is_todo;
        }
    );
}

sub worst_case_balance {
    my ($self, $start, $offset, $length) = @_;
    return $self->balance(
        start => $start,
        offset => $offset,
        length => $length,
        filter => sub {
            my $entry = shift();
            return 0 if $entry->is_future;
            return 0 if $entry->is_todo;
            return ($entry->amount < 0) || $entry->is_posted;
        }
    );
}

sub future_balance {
    my ($self, $start, $offset, $length) = @_;
    return $self->balance(
        start => $start,
        offset => $offset,
        length => $length
    );
}

sub parse_amount {
    my ($amount) = @_;
    if ($amount =~ m{^$RX_AMOUNT$}xi) {
        $amount =~ s{\$+}{}g;
        if ($amount =~ m{^\((.*)\)$}) {
            return 0.0 + $1;    # deposit/credit (casting to double)
        } else {
            return 0.0 - $amount; # withdrawal/debit (casting to double)
        }
    }
    return undef;
}

sub process_parsed_line {
    my ($self, $line, $entry, $flag, $date, $amount, $merchant) = @_;

    warn("<< $amount\n") if eval { $ENV{DEBUG} && $ENV{DEBUG} >= 2 };
    $amount = parse_amount($amount);
    warn(">> $amount\n") if eval { $ENV{DEBUG} && $ENV{DEBUG} >= 2 };
    if ($merchant =~ m{^\**\s*STARTING BALANCE\s*\**$}) {
        if ($self->start_balance) {
            warn("Second starting balance specified at $ARGV line $.\n");
        }
        if (defined $amount) {
            $self->start_balance(-$amount);
        } else {
            $self->start_balance(0);
        }
        return;
    }
    if (defined $amount) {
        $entry->amount($amount);
    } else {
        $entry->amount(undef);
    }
    if ($merchant =~ m{\[\s*p(?:reauth|end(?:ing)?)?\s+
                       ($RX_AMOUNT)
                       \s*\]}xi) {
        warn("<<< $1\n") if eval { $ENV{DEBUG} && $ENV{DEBUG} >= 2 };
        $entry->pending_amount(parse_amount($1));
        warn(">>> " . $entry->pending_amount . "\n") if eval { $ENV{DEBUG} && $ENV{DEBUG} >= 2 };
    }
    if ($merchant =~ m{\[\s*
                       ($RX_AMOUNT)
                       \s*\]}xi) {
        $entry->pending_amount(parse_amount($1));
    }
    my $parse_date = $self->parse_date($date);
    my $date_fmt;
    if (defined $parse_date) {
        $date_fmt = strftime("%Y-%m-%d", localtime($parse_date));
    } else {
        warn(sprintf("Cannot parse date %s at %s line %s.\n",
                     $date, $ARGV, $.));
    }

    if ($self->strip_comments) {
        if ($self->comment_style eq 'c++') {
            $merchant =~ s{\s*//.*\z}{};
        }
    }

    $self->_last_date($parse_date);
    $self->_last_date_fmt($date_fmt);
    $entry->date($parse_date);
    $entry->date_fmt($date_fmt);
    $entry->merchant($merchant);

    my $flag_out;
    if (defined($flag) and ($flag ne "")) {
        my $length = length($flag);
        $flag = substr($flag, 0, 1);
        $entry->flag_out($flag x $length);
        if ($flag eq "-") {
            $entry->is_pending(1);
        } elsif ($flag eq "*" or $flag eq "/") {
            $entry->is_posted(1);
        } elsif ($flag eq "+") {
            $entry->is_future(1);
        } elsif ($flag eq "!") {
            $entry->is_todo(1);
        } elsif ($flag eq ".") {
            # regular entry, not posted, act as if there is no flag
            # character at all
        }
    }

    my $normalized_text = sprintf(
        "%-7s %-15s %-15s %s",
        $entry->get_flag_character,
        $entry->date_fmt,
        $entry->get_amount_fmt,
        $entry->merchant
    );

    if ($line->{text} =~ m{\t}) {
        $normalized_text = unexpand($normalized_text);
    }

    $line->{normalized_text} = $normalized_text;
    $line->{entry}           = $entry;
    $line->{flag_out}        = $flag_out;

    push(@{$self->entries}, $entry);
}

sub parse_line {
    my ($self, $text) = @_;

    return if $self->_stop_parsing;

    local $_ = $text;

    my $line_number = scalar(@{$self->lines}) + 1;
    my $line = My::Transaction::Register::Line->new(
        line_number => $line_number,
        text        => $_
    );
    push(@{$self->lines}, $line);

    my $entry = My::Transaction::Register::Entry->new(
        line_number => $line_number,
        text        => $_
    );

    return if /^\s*\#/;
    return unless /\S/;

    if (m{^\s*\**\s*CHECKPOINT\s*\**\s*}i) {
        my $note = $';
        warn("checkpoint\n") if $ENV{DEBUG};
        my $checkpoint = My::Transaction::Register::Checkpoint->new(
            date => $self->_last_date,
            date_fmt => $self->_last_date_fmt,
            running_balance => $self->running_balance(),
            note => $note
        );
        push(@{$self->checkpoints}, $checkpoint);
        $line->{type} = 'checkpoint';
        $line->{data} = $checkpoint;
        return;
    }
    if (m{^\s*\**\s*STOP\s*\**\s*}i) {
        $self->_stop_parsing(1);
        $line->{type} = 'stop-parsing';
        return;
    }
    if (m{^\s*(\S+)\s*=\s*(.*?)\s*$}i) {
        my ($name, $value) = ($1, $2);
        if ($name eq "account-type") {
            $self->account_type($value);
            $line->{type} = 'account-type';
            $line->{data} = $value;
        } elsif ($name eq "starting-balance") {
            if ($self->start_balance) {
                warn("Second starting balance specified at $ARGV line $.\n");
            }
            $self->start_balance($value);
            $line->{type} = 'start-balance';
            $line->{data} = $value;
        } elsif ($name eq "starting-date") {
            my $parse_date = $self->parse_date_absolute($value);
            if (!defined $parse_date) {
                die(sprintf("invalid date '%s' at %s line %d\n",
                            $value,
                            $ARGV,
                            $.));
            }
            $self->start_date($parse_date);
            $line->{type} = 'start-date';
            $line->{data} = $parse_date;
            $self->_last_date($parse_date);
            $self->_last_date_fmt(strftime('%Y-%m-%d', localtime($parse_date)));
        }
        return;
    }

    if ($_ =~ $RX_LINE_1) {
        my ($flag, $date, $amount, $merchant) = ($1, $2, $3, $4);
        $self->process_parsed_line($line, $entry, $flag, $date, $amount, $merchant);
        return;
    }
    if ($_ =~ $RX_LINE_2) {
        my ($flag, $date, $merchant, $amount) = ($1, $2, $3, $4);
        $self->process_parsed_line($line, $entry, $flag, $date, $amount, $merchant);
        return;
    }

    die("Funny-looking line at $ARGV line $.\n");
}

use Text::ASCIITable;

sub debug {
    my ($self) = @_;
    my $running;
    my $pending;
    my $posted;
    my $worst_case;
    my $future;

    my @cols = qw(Number Date Amount Running Pending Ledger Worst-Case Future To-Do);

    my $t = Text::ASCIITable->new();
    $t->setCols(@cols);

    $t->addRow("", "", "", (map { sprintf("%.2f", $_) } (($self->start_balance) x 5)));
    $t->addRowLine();

    for (my $i = 0; $i < scalar(@{$self->entries}); ++$i) {
        $running    = $self->running_balance($running, $i, 1);
        $pending    = $self->pending_balance($pending, $i, 1);
        $posted     = $self->posted_balance($posted, $i, 1);
        $worst_case = $self->worst_case_balance($worst_case, $i, 1);
        $future     = $self->future_balance($future, $i, 1);
        $t->addRow($i,
                   $self->entries->[$i]->date_fmt,
                   sprintf("%.2f", $self->entries->[$i]->amount),
                   (map { sprintf("%.2f", $_) }
                      ($running, $pending, $posted, $worst_case, $future)));
    }

    $t->addRowLine();
    $t->addRow(@cols);

    print($t);
}

use Data::Dumper qw();

sub dump {
    my ($self) = @_;
    my $o = {
        entries => $self->{entries},
        balance => {
            posted     => $self->posted_balance(),
            pending    => $self->pending_balance(),
            running    => $self->running_balance(),
            worst_case => $self->worst_case_balance(),
            future     => $self->future_balance(),
        },
    };
    local $Data::Dumper::Indent = 1;
    local $Data::Dumper::Terse = 1;
    local $Data::Dumper::Deepcopy = 1;
    local $Data::Dumper::Sortkeys = 1;
    print Data::Dumper::Dumper($o);
}

sub output_summary {
    my ($self) = @_;
    printf("    Ledger balance: \$%9.2f  (includes posted transactions)\n",
           $self->posted_balance());
    print( "                                CONFIRM: LEDGER BALANCE\n");
    printf("   Pending balance: \$%9.2f  (includes pending transactions)\n",
           $self->pending_balance());
    print( "                                CONFIRM: AVAILABLE BALANCE\n");
    printf("   Running balance: \$%9.2f  (includes all listed transactions)\n",
           $self->running_balance());
    printf("Worst-case balance: \$%9.2f  (excludes deposits not posted)\n",
           $self->worst_case_balance());
    printf("    Future balance: \$%9.2f  (running balance then future and to-do transactions)\n",
           $self->future_balance());
}

sub output_checkpoints {
    my ($self) = @_;
    if (scalar(@{$self->checkpoints})) {
        print("Checkpoints:\n");
        foreach my $checkpoint (@{$self->checkpoints}) {
            printf("  \$%9.2f  %s  %s\n",
                   $checkpoint->running_balance,
                   $checkpoint->date_fmt,
                   $checkpoint->note);
        }
    }
}

sub output_as_string {
    my ($self) = @_;
    foreach my $line (@{$self->lines}) {
        print $line->{normalized_text} // $line->{text};
        print "\n";
    }
}

use Time::ParseDate qw();

use constant SIX_MONTHS_FUTURE => 60 * 60 * 24 * 183;

sub parse_date {
    my ($self, $date) = @_;

    # prefer a date that's between (six months ago) and (six months
    # from now).
    if (defined $self->_last_date) {
        return Time::ParseDate::parsedate(
            $date,
            PREFER_PAST => 1,
            NOW => $self->_last_date + SIX_MONTHS_FUTURE
        );
    }
    return Time::ParseDate::parsedate(
        $date,
        PREFER_PAST => 1,
        NOW => time() + SIX_MONTHS_FUTURE
    );
}

sub parse_date_absolute {
    my ($self, $date) = @_;
    my $result = Time::ParseDate::parsedate($date);
    return $result;
}

# use Finance::OFX;
use XML::LibXML;
use URI::Escape qw(uri_escape);

sub list_ofx_institutions {
    my ($self, $search) = @_;
    my $url = $self->get_ofx_search_url($search);
    my $doc = $self->get_xml_doc($url);
    if (!$doc) {
        return wantarray ? () : undef;
    }
    my @nodes = $doc->findnodes("/institutionlist/institutionid");
    my @result;
    foreach my $node (@nodes) {
        push(@result, My::Transaction::Register::Institution->new(
            id   => $node->getAttribute("id"),
            name => $node->getAttribute("name")
        ));
    }
    if (wantarray) {
        return @result;
    } else {
        return \@result;
    }
}

sub get_ofx_search_url {
    my ($self, $search) = @_;
    my $url;
    if (defined $search) {
        $url = sprintf("http://www.ofxhome.com/api.php?search=%s", uri_escape($search));
    } else {
        $url = "http://www.ofxhome.com/api.php?all=yes";
    }
    return $url;
}

sub get_ofx_institution_info {
    my ($self, $id) = @_;
    my $request_url = sprintf("http://www.ofxhome.com/api.php?lookup=%s", uri_escape($id));
    my $doc = $self->get_xml_doc($request_url);
    if (!$doc) {
        return undef;
    }
    my $node = eval { $doc->findnodes("/institution")->[0]; };
    if (!$node) {
        return undef;
    }
    print($doc->toString());

    $id                   = $node->getAttribute("id");

    my $name              = eval { $node->findnodes("name")->[0]->textContent };
    my $fid               = eval { $node->findnodes("fid")->[0]->textContent };
    my $org               = eval { $node->findnodes("org")->[0]->textContent };
    my $url               = eval { $node->findnodes("url")->[0]->textContent };
    my $brokerid          = eval { $node->findnodes("brokerid")->[0]->textContent };
    my $ofxfail           = eval { $node->findnodes("ofxfail")->[0]->textContent };
    my $sslfail           = eval { $node->findnodes("sslfail")->[0]->textContent };
    my $lastofxvalidation = eval { $node->findnodes("lastofxvalidation")->[0]->textContent };
    my $lastsslvalidation = eval { $node->findnodes("lastsslvalidation")->[0]->textContent };
    my $notes             = eval { $node->findnodes("notes")->[0]->textContent };

    my $profile           = eval { $node->findnodes("profile")->[0] };

    my $profile_addr1      = eval { $profile->getAttribute("addr1") };
    my $profile_addr2      = eval { $profile->getAttribute("addr2") };
    my $profile_addr3      = eval { $profile->getAttribute("addr3") };
    my $profile_city       = eval { $profile->getAttribute("city") };
    my $profile_state      = eval { $profile->getAttribute("state") };
    my $profile_postalcode = eval { $profile->getAttribute("postalcode") };
    my $profile_country    = eval { $profile->getAttribute("country") };
    my $profile_url        = eval { $profile->getAttribute("url") };

    return My::Transaction::Register::InstitutionInfo->new(
        id                => $id,
        name              => $name,
        fid               => $fid,
        org               => $org,
        url               => $url,
        brokerid          => $brokerid,
        ofxfail           => $ofxfail,
        sslfail           => $sslfail,
        lastofxvalidation => $lastofxvalidation,
        lastsslvalidation => $lastsslvalidation,
        notes             => $notes,
        profile => My::Transaction::Register::InstitutionInfo::Profile->new(
            addr1      => $profile_addr1,
            addr2      => $profile_addr2,
            addr3      => $profile_addr3,
            city       => $profile_city,
            state      => $profile_state,
            postalcode => $profile_postalcode,
            country    => $profile_country,
            url        => $profile_url
        )
    );
}

sub get {
    my ($self, $url) = @_;
    my $ua = My::Transaction::Register::UserAgent->new();
    my $request = HTTP::Request->new("GET", $url);
    my $response = $ua->request($request);
    return $response;
}

sub get_xml_doc {
    my ($self, $url) = @_;
    my $response = $self->get($url);
    if (!$response->is_success) {
        return undef;
    }
    my $xml = $response->decoded_content;
    my $doc = XML::LibXML->load_xml(string => $xml);
    return $doc;
}

package My::Transaction::Register::Institution;
use warnings;
use strict;

use Moose;
has 'id'   => (is => 'rw', isa => 'Int', required => 1);
has 'name' => (is => 'rw', isa => 'Str', required => 1);

package My::Transaction::Register::InstitutionInfo;
use warnings;
use strict;

use Moose;
has 'id'                => (is => 'rw', isa => 'Int', required => 1);
has 'name'              => (is => 'rw', isa => 'Str', required => 1);
has 'fid'               => (is => 'rw', isa => 'Int', required => 1);
has 'org'               => (is => 'rw', isa => 'Str', required => 1);
has 'url'               => (is => 'rw', isa => 'Str', required => 1);
has 'brokerid'          => (is => 'rw', isa => 'Maybe[Int]', required => 0);
has 'ofxfail'           => (is => 'rw', isa => 'Bool', required => 1);
has 'sslfail'           => (is => 'rw', isa => 'Bool', required => 1);
has 'lastofxvalidation' => (is => 'rw', isa => 'Str', required => 1);
has 'lastsslvalidation' => (is => 'rw', isa => 'Str', required => 1);
has 'profile'           => (is => 'rw', isa => 'My::Transaction::Register::InstitutionInfo::Profile', required => 1);
has 'notes'             => (is => 'rw', isa => 'Maybe[Str]', required => 0);

package My::Transaction::Register::InstitutionInfo::Profile;
use warnings;
use strict;

use Moose;
has 'addr1'      => (is => 'rw', isa => 'Maybe[Str]', required => 1);
has 'addr2'      => (is => 'rw', isa => 'Maybe[Str]', required => 0);
has 'addr3'      => (is => 'rw', isa => 'Maybe[Str]', required => 0);
has 'city'       => (is => 'rw', isa => 'Maybe[Str]', required => 1);
has 'state'      => (is => 'rw', isa => 'Maybe[Str]', required => 1);
has 'postalcode' => (is => 'rw', isa => 'Maybe[Str]', required => 1);
has 'country'    => (is => 'rw', isa => 'Maybe[Str]', required => 1);
has 'url'        => (is => 'rw', isa => 'Maybe[Str]', required => 1);

# signonmsgset="true" bankmsgset="true" billpaymsgset="true"
# emailmsgset="true"/>


package My::Transaction::Register::UserAgent;
use warnings;
use strict;

use base "LWP::UserAgent";

package My::Transaction::Register::Entry;
use warnings;
use strict;

use Moose;
has 'is_future'      => (is => 'rw', isa => 'Bool', default => 0);
has 'is_pending'     => (is => 'rw', isa => 'Bool', default => 0);
has 'is_posted'      => (is => 'rw', isa => 'Bool', default => 0);
has 'is_todo'        => (is => 'rw', isa => 'Bool', default => 0);
has 'pending_amount' => (is => 'rw', isa => 'Num|Undef', required => 0, default => 0);
has 'amount'         => (is => 'rw', isa => 'Num|Undef', required => 0, default => 0);
has 'date'           => (is => 'rw', isa => 'Str|Undef');
has 'date_fmt'       => (is => 'rw', isa => 'Str|Undef');
has 'merchant'       => (is => 'rw', isa => 'Str');
has 'flag_out'       => (is => 'rw', isa => 'Str|Undef', required => 0);

sub get_flag_character {
    my ($self) = @_;
    return $self->flag_out if defined $self->flag_out;
    return '-' if $self->is_pending;
    return '/' if $self->is_posted;
    return '+' if $self->is_future;
    return '!' if $self->is_todo;
    return '.';
}

sub get_amount_fmt {
    my ($self) = @_;
    if (defined $self->amount) {
        return sprintf('%.2f', -$self->amount) if $self->amount < 0;
        return sprintf('(%.2f)', $self->amount);
    }
    return '???';
}

package My::Transaction::Register::Line;
use warnings;
use strict;

use Moose;
has 'line_number' => (is => 'rw', isa => 'Int');
has 'text'        => (is => 'rw', isa => 'Str');

package My::Transaction::Register::Checkpoint;
use warnings;
use strict;

use Moose;
has 'date'            => (is => 'rw', isa => 'Str');
has 'date_fmt'        => (is => 'rw', isa => 'Str');
has 'running_balance' => (is => 'rw', isa => 'Num');
has 'note'            => (is => 'rw', isa => 'Str');

1;

