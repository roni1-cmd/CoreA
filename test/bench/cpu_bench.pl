#!/usr/bin/perl
# CoreA CPU Benchmark (Perl)
# Measures CPU performance with arithmetic loop

use strict;
use warnings;
use CoreA::Syscall;

my $iterations = 1000000;
my $result = 0;
for my $i (1..$iterations) {
    $result += $i * 2;
}
sys_write(1, "CPU Benchmark Result: $result\n", length("CPU Benchmark Result: $result\n"));
sys_exit(0);
