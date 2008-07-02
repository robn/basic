#!/usr/bin/perl

use 5.010;
use warnings;
use strict;

use Parse::RecDescent;
use IO::Prompt;
use Data::Dumper;

my %handlers = (
    continue_statement => \&continue_handler,
    clr_statement      => \&clr_handler,
    data_statement     => \&data_handler,
    dim_statement      => \&dim_handler,
    end_statement      => \&end_handler,
    for_statement      => \&for_handler,
    goto_statement     => \&goto_handler,
    gosub_statement    => \&gosub_handler,
    if_statement       => \&if_handler,
    input_statement    => \&input_handler,
    let_statement      => \&let_handler,
    next_statement     => \&next_handler,
    new_statement      => \&new_handler,
    print_statement    => \&print_handler,
    read_statement     => \&read_handler,
    rem_statement      => \&rem_handler,
    restore_statement  => \&restore_handler,
    return_statement   => \&return_handler,
    run_statement      => \&run_handler,
    stop_statement     => \&stop_handler,
);

my %cmp_operators = (
    "="  => sub { $_[0] == $_[1] },
    "<>" => sub { $_[0] != $_[1] },
    "<"  => sub { $_[0] <  $_[1] },
    ">"  => sub { $_[0] >  $_[1] },
    "<=" => sub { $_[0] <= $_[1] },
    ">=" => sub { $_[0] >= $_[1] },
);

my %add_operators = (
    "+"  => sub { $_[0] + $_[1] },
    "-"  => sub { $_[0] - $_[1] },
);

my %mult_operators = (
    "*"  => sub { $_[0] * $_[1] },
    "/"  => sub { $_[0] / $_[1] },
);

my %unary_operators = (
    "NOT" => sub { ! $_[0] },
    "-"   => sub { - $_[0] },
);

my $p = do { local $/; Parse::RecDescent->new(<DATA>) };

my %lines;
my %vars;
my $pc;

while (my $raw = prompt "basic> ", -line) {
    $raw = "$raw";
    my $line = $p->line($raw);
    if (!$line) {
        print "parse error\n";
        next;
    }

    if ($line->{immediate_line}) {
        execute($line->{immediate_line}->{"statement(s)"});
        next;
    }

    if ($line->{program_line}) {
        my $lineno = $line->{program_line}->{line_number}->{__VALUE__};
        $lines{$lineno} = $line->{program_line}->{"statement(s)"};
        next;
    }

    if ($line->{delete_line}) {
        my $lineno = $line->{delete_line}->{line_number}->{__VALUE__};
        delete $lines{$lineno};
        next;
    }
}

sub execute {
    my ($statements) = @_;

    for my $st (@{$statements}) {
        my ($type) = grep { /_statement$/ } keys %{$st};
        $handlers{$type}->($st->{$type});
    }
}

sub execute_from {
    return if scalar keys %lines == 0;

    my @linenos = sort { $a <=> $b } keys %lines;

    my $_find_array_pos;
    $_find_array_pos = sub {
        my ($want, $pos, $len) = @_;

        my $cur = $pos+$len/2;

        return $cur if $linenos[$cur] == $want;
        return $_find_array_pos->($want, 0, $cur) if $linenos[$cur] < $want;
        return $_find_array_pos->($want, $cur, scalar @linenos) if $linenos[$cur] < $want;

        return;
    };

    $pc = $_[0] // undef;
    my $pos;

    if (not defined $pc) {
        $pc = $linenos[0];
        $pos = 0;
    }
    else {
        $pos = $_find_array_pos->($pc, 0, $#linenos);
        return if not defined $pos;
    }

    while (defined $pc) {
        my $curpc = $pc;
        execute($lines{$curpc});

        if ($pc == $curpc) {
            return if $pos == $#linenos;

            $pc = $linenos[$pos++];
        }

        elsif (defined $pc) {
            $pos = $_find_array_pos->($pc, 0, $#linenos);
            return if not defined $pos;
        }

        else {
            return;
        }
    }
}

sub expand_expression {
    my ($e) = @_;

    if ($e->isa("expression")) {
        return expand_expression($e->{or_expression});
    }

    if ($e->isa("or_expression")) {
        my @terms = ( (map { $_->{and_expression} }
                           @{$e->{"_alternation_1_of_production_1_of_rule_or_expression(s?)"}}),
                      $e->{and_expression});

        my $value = expand_expression(shift @terms);
        while (@terms > 0) { $value |= expand_expression(shift @terms) };

        return $value;
    }

    if ($e->isa("and_expression")) {
        my @terms = ( (map { $_->{comparative_expression} }
                           @{$e->{"_alternation_1_of_production_1_of_rule_and_expression(s?)"}}),
                      $e->{comparative_expression});
        
        my $value = expand_expression(shift @terms);
        while (@terms > 0) { $value &= expand_expression(shift @terms) };

        return $value;
    }

    if ($e->isa("comparative_expression")) {
        my @expr = ( (map { ($_->{additive_expression}, $_->{cmp_operator}) }
                          @{$e->{"_alternation_1_of_production_1_of_rule_comparative_expression(s?)"}}),
                      $e->{additive_expression});

        my $value = expand_expression(shift @expr);
        while (@expr > 0) {
            my $op = shift @expr;
            $value = $cmp_operators{$op->{__VALUE__}}->($value, expand_expression(shift @expr));
        }

        return $value;
    }

    if ($e->isa("additive_expression")) {
        my @expr = ( (map { ($_->{multiplicative_expression}, $_->{add_operator}) }
                          @{$e->{"_alternation_1_of_production_1_of_rule_additive_expression(s?)"}}),
                      $e->{multiplicative_expression});

        my $value = expand_expression(shift @expr);
        while (@expr > 0) {
            my $op = shift @expr;
            $value = $add_operators{$op->{__VALUE__}}->($value, expand_expression(shift @expr));
        }

        return $value;
    }

    if ($e->isa("multiplicative_expression")) {
        my @expr = ( (map { ($_->{unary_expression}, $_->{mult_operator}) }
                          @{$e->{"_alternation_1_of_production_1_of_rule_multiplicative_expression(s?)"}}),
                      $e->{unary_expression});

        my $value = expand_expression(shift @expr);
        while (@expr > 0) {
            my $op = shift @expr;
            $value = $mult_operators{$op->{__VALUE__}}->($value, expand_expression(shift @expr));
        }

        return $value;
    }

    if ($e->isa("unary_expression")) {
        if (exists $e->{primary_expression}) {
            return expand_expression($e->{primary_expression});
        }

        if (exists $e->{unary_operator}->{NOT}) {
            return $unary_operators{NOT}->(expand_expression($e->{unary_expression}));
        }

        return $unary_operators{$e->{unary_operator}->{__VALUE__}}->(expand_expression($e->{unary_expression}));
    }

    if ($e->isa("primary_expression")) {
        if (exists $e->{literal}) {
            if (exists $e->{literal}->{integer}) {
                return basic::integer->new($e->{literal}->{integer}->{__VALUE__});
            }

            if (exists $e->{literal}->{string}) {
                return basic::string->new($e->{literal}->{string}->{__PATTERN1__});
            }
        }

        if (exists $e->{identifier}) {
            my ($key, $type) = identifier($e->{identifier});

            return $vars{$key} ||= $type->new;
        }

        if (exists $e->{expression}) {
            return expand_expression($e->{expression});
        }
    }

    return "";
}

sub identifier {
    my ($tree) = @_;

    my $identifier = $tree->{__PATTERN1__};
    my $sigil = $tree->{"sigil(?)"}->[0]->{__VALUE__} || "";

    my $key = "$identifier$sigil";
    my $type = $sigil eq '$' ? "basic::string" : "basic::integer";

    return ($key, $type);
}

sub input_handler {
    my ($st) = @_;

    my @inputs;

    for my $tree (@{$st->{"identifier(s)"}}) {
        my ($key, $type) = identifier($tree);

        if (!@inputs) {
            @inputs = split ',', prompt "? ";
        }

        if (@inputs) {
            $vars{$key} = $type->new(shift @inputs);
            next;
        }

    }
}

sub let_handler {
    my ($st) = @_;

    my ($key, $type) = identifier($st->{identifier});

    my $value = expand_expression($st->{expression});

    if (!$value->isa($type)) {
        print "type mismatch\n";
        return;
    }

    $vars{$key} = $value;
}

sub print_handler {
    my ($st) = @_;

    my $out = "";

    if ($st->{"expression(s?)"}) {
        for my $e (@{$st->{"expression(s?)"}}) {
            $out .= expand_expression($e);
        }
    }

    print "$out\n";
}

sub run_handler {
    my ($st) = @_;

    execute_from();
}


package overload;

sub fixup {
    my ($a, $b, $swap) = @_;

    $a = ref($a) ? ${$a} : $a;
    $b = ref($b) ? ${$b} : $b;

    return ($b, $a) if $swap;
    return ($a, $b);
}


package basic::integer;

use overload
    '==' => sub { my ($a, $b) = overload::fixup(@_); $a == $b },
    '!=' => sub { my ($a, $b) = overload::fixup(@_); $a != $b },
    '<'  => sub { my ($a, $b) = overload::fixup(@_); $a <  $b },
    '>'  => sub { my ($a, $b) = overload::fixup(@_); $a >  $b },
    '<=' => sub { my ($a, $b) = overload::fixup(@_); $a <= $b },
    '>=' => sub { my ($a, $b) = overload::fixup(@_); $a >= $b },

    '+'  => sub { my ($a, $b) = overload::fixup(@_); basic::integer->new($a + $b) },
    '-'  => sub { my ($a, $b) = overload::fixup(@_); basic::integer->new($a - $b) },
    '*'  => sub { my ($a, $b) = overload::fixup(@_); basic::integer->new($a * $b) },
    '/'  => sub { my ($a, $b) = overload::fixup(@_); basic::integer->new($a / $b) },

    '!'  => sub { basic::integer->new(!${$_[0]}) },

    '""' => sub { "${$_[0]}" };

use Carp;

sub new {
    my ($class, $value) = @_;
    $value = 0 if @_ == 1;

    croak "value is not an integer" if $value !~ m/^[+-]?\d+$/;

    return bless \$value, $class;
}


package basic::string;

use overload
    '==' => sub { my ($a, $b) = overload::fixup(@_); $a eq $b },
    '!=' => sub { my ($a, $b) = overload::fixup(@_); $a ne $b },
    '<'  => sub { my ($a, $b) = overload::fixup(@_); $a lt $b },
    '>'  => sub { my ($a, $b) = overload::fixup(@_); $a gt $b },
    '<=' => sub { my ($a, $b) = overload::fixup(@_); $a le $b },
    '>=' => sub { my ($a, $b) = overload::fixup(@_); $a ge $b },

    '+'  => sub { my ($a, $b) = overload::fixup(@_); basic::string->new($a . $b) },

    '""' => sub { "${$_[0]}" };

sub new {
    my ($class, $value) = @_;
    $value = "" if @_ == 1;
    return bless \$value, $class;
}


package main;

__DATA__

<autotree>

line: immediate_line | program_line | delete_line

immediate_line: statement(s /:/) /\Z/
program_line: line_number statement(s /:/) /\Z/
delete_line: line_number /\Z/

line_number: /\d+/

continue_statement: CONTINUE
clr_statement:      CLR
data_statement:     DATA literal(s /,/)
dim_statement:      DIM identifier "(" integer(s /,/) ")"
end_statement:      xEND
for_statement:      FOR identifier "=" expression TO expression STEP integer
goto_statement:     GOTO expression
gosub_statement:    GOSUB expression
if_statement:       IF expression THEN statement
input_statement:    INPUT identifier(s /,/)
let_statement:      LET(?) identifier "=" expression
next_statement:     NEXT identifier
new_statement:      NEW
print_statement:    PRINT expression(s? /;/)
read_statement:     READ identifier(s)
rem_statement:      REM /.*/
restore_statement:  RESTORE
return_statement:   RETURN
run_statement:      RUN
stop_statement:     STOP

statement: continue_statement
         | clr_statement
         | data_statement
         | dim_statement
         | end_statement
         | for_statement
         | goto_statement
         | gosub_statement
         | if_statement
         | input_statement
         | let_statement
         | next_statement
         | new_statement
         | print_statement
         | read_statement
         | rem_statement
         | restore_statement
         | return_statement
         | run_statement
         | stop_statement

sigil: '$'

identifier: /[a-zA-Z](?:[a-zA-Z0-9])*/ sigil(?)

string: '"' <skip:''> /(?:\\"|[^"])*/ '"'
integer: /-?\d+/

literal: integer | string

primary_expression: identifier
                  | literal
                  | "(" expression ")"

unary_expression: unary_operator unary_expression
                | primary_expression
unary_operator: NOT | "-"

multiplicative_expression: (unary_expression mult_operator)(s?) unary_expression
mult_operator: "*" | "/"

additive_expression: (multiplicative_expression add_operator)(s?) multiplicative_expression
add_operator: "+" | "-"

comparative_expression: (additive_expression cmp_operator)(s?) additive_expression
cmp_operator: "=" | "<>" | "<" | ">" | "<=" | ">="

and_expression: (comparative_expression AND)(s?) comparative_expression
or_expression: (and_expression OR)(s?) and_expression

expression: or_expression

CONTINUE: /CONTINUE/i
CLR:      /CLR/i
DATA:     /DATA/i
DIM:      /DIM/i
xEND:     /END/i
FOR:      /FOR/i
GOTO:     /GOTO/i
GOSUB:    /GOSUB/i
IF:       /IF/i
INPUT:    /INPUT/i
LET:      /LET/i
NEXT:     /NEXT/i
NEW:      /NEW/i
PRINT:    /PRINT/i
READ:     /READ/i
REM:      /REM/i
RESTORE:  /RESTORE/i
RETURN:   /RETURN/i
RUN:      /RUN/i
STOP:     /STOP/i

STEP:     /STEP/i
THEN:     /THEN/i
TO:       /TO/i

AND:      /AND/i
NOT:      /NOT/i
OR:       /OR/i
