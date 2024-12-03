#!/usr/bin/perl

use strict;
use warnings;

open(my $infile, "<", "3.input") or die "Can't open input file: $!";
my $input = join("\n", <$infile>);

my @captures = $input =~ /mul\((\d+),(\d+)\)/gs;

my $res = 0;

for (my $i = 0; $i < @captures; $i += 2) {
    $res += $captures[$i] * $captures[$i + 1];
}

$input =~ s/don't\(\)((?!do\(\)).)*$//s;
$input =~ s/don't\(\).*?do\(\)//gs;

@captures = $input =~ /mul\((\d+),(\d+)\)/gs;

my $res2 = 0;

for (my $i = 0; $i < @captures; $i += 2) {
    $res2 += $captures[$i] * $captures[$i + 1];
}

print "$res\n$res2\n";
