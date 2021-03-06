# transaction-register-perl

## What is this?

A simple program and Perl module for working with text-file checkbook
registers whose format is described below.  It's primary for my
personal use but you can use it too.

NOTE: This is not a double-entry accounting system.  If you want one,
visit the [Plain Text Accounting](http://plaintextaccounting.org/) web
site. The most popular is [Ledger](http://ledger-cli.org/), invented
by the guy who now maintains
[Emacs](https://www.gnu.org/software/emacs/).  :-)

## How to run?

    transaction-register check [FILENAME ...]

Then confirm that the ledger balance and pending balance listed in the
output match with your online banking.

And/or just confirm your balance isn't in the negative.

## Text File Format Example

    account-type     = checking
    starting-balance = 6511.98
    starting-date    = 2017-01-01
    
    # A comment.
        # Another comment.

    /       01/01   (2250.00)       work
    /       01/03   145.50          car insurance
    /       01/03   318.93          car payment
    /       01/03   79.45           phone bill
    -       01/06   60.00           atm
    -       01/06   5.98            taco bell
            01/08   76.99           cable bill
            01/08   25.97           chi-chi's [preauth 20.97]
            01/08   (20.00)         payment from some guy on ebay
    +       01/15   (2250.00)       work
    +       01/15   232.50          amex credit card payment
    !       01/15   798.53          #2602: mortgage
    +       01/23   ???             LevelUp

## Output Example

        Ledger balance: $  8218.10  (includes posted transactions)
                                    CONFIRM: LEDGER BALANCE
       Pending balance: $  8152.12  (includes pending transactions)
                                    CONFIRM: AVAILABLE BALANCE
       Running balance: $  8069.16  (includes all listed transactions)
    Worst-case balance: $  8049.16  (excludes deposits not posted)
        Future balance: $  9288.13  (running balance then future and to-do transactions)

In this example, your balances would be:

    8218.10 - ledger balance (includes just the posted
              transactions)

    8152.12 - pending balance (adds the pending
              transactions)

    8069.16 - running balance (adds the transactions
              you've made but not yet marked as
              pending or posted)

    8049.16 - worst-case balance (your running balance,
              excluding deposits not yet posted)

    9288.13 - future balance (also includes transactions
              marked with the future or to-do flag, i.e., you
              plan to make them but haven't yet done so,
              or automatic transactions yet to take place)
              
## Text File Format Description

Blank lines, lines containing only whitespace, and lines starting with
`#` (with or without optional whitespace before) are ignored.

You can also have <code><var>name</var> = <var>value</var></code>
lines.  See example above.  You want to specify these before your
transactions.  The whitespace at the beginning and end of the line,
and the whitespace before and after the equal sign, are optional.
Currently supported values for <code><var>name</var></code>:

    account-type       Can be checking, credit, or credit-card.
    starting-balance   Specify a dollar amount.
    starting-date      Specify a date.  Currently not interpreted in
                       any way.

Each line can also specify a transaction.  Transactions specify the
date, then the amount, then a description field.  The fields are
separated by whitespaces.  The description field is the last field,
and may itself contain whitespaces.

You can also put one of the following as an additional
whitespace-separated field before the remaining fields:

    /   posted to your account
    -   pending - listed as "pending" in your account
    +   future - transactions you haven't made yet,
        typically for automatic transactions
    !   to-do - transactions you haven't made yet,
        typically for checks you need to write
    .   recorded - an optional 'flag' indicating the
        transaction isn't flagged.  This is useful
        to enable running `sort -k2`.
    
You may also prefix the line with whitespaces.

Amounts are specified as follows:

-   Regular amounts are for the default transaction type
    (withdrawals for checking accounts).
-   Negative or parentheses-surrounded amounts are the
    opposite type (deposits for checking accounts).
-   Dollar sign `$` is optional.
-   Must contain two decimal places.

Examples:

    123.45
    $123.45

Negative amounts:

    (123.45)
    $(123.45)
    ($123.45)
    -$123.45
    $-123.45

You can also use one or more '?' characters to specify an unknown
amount:

    ???

Dates are specified in any format compatible with `Time::ParseDate`
that does not contain whitespace.  Examples:

	06/20/2017
	06/20
	2017-06-20
	06-20

If the year is not specified it is relative to the previously
specified transaction date, or the starting-date, or the current time,
whichever is defined first.

With `account-type = checking`, transaction amounts are normally
debits.  In parentheses they are credits.  (That might be backwards to
some, but at least in my case most of my transactions are debits so I
wanted that to be the default.)  Dollar signs are optional.  Two-digit
cents are required.

To specify a check, start the description field with
<code>#<var>number</var></code> or <code>#<var>number</var>:</code>
specifying the check number.  It will not be interpreted as a comment,
since the `#` is not at the start of the line (with optional
whitespace preceeding).

You can specify <code>[preauth <var>amount</var>]</code> somewhere in
the description field.  The pending balance this program outputs will
then take into account the preauth amount, not the final amount, if
the transaction is flagged as pending.  Typically I use this for
sit-down restaurant transactions, where preauth amounts don&rsquo;t
include tips.



## TODO

Modify the module to support any Time::ParseDate-compatible format
that doesn't contain whitespace.

Sync with any of the following file formats as exported from your
online banking:

    .qfx   Quicken
    .qbo   Quickbooks
    .ofx   Open Financial Exchange (used by Microsoft Money?)
    .csv   Microsoft Excel (maybe?)

Sync with online banking.  Working example code is hard to find
though.

When syncing, modify files in-place, keeping changes to a minimum.

## Miscellaneous

There is code in `My::Transaction::Register` that supports things I
did not mention here.
