#!/usr/bin/perl
# CoreA Sensor Reader (Perl)
# Reads simulated sensor data, uses sys_fork

use strict;
use warnings;
use Inline C => Config => LIBS => '-lsyscall';
use Inline C => <<'END_C';
#include <syscall.h>
void sys_write(int fd, const char *buf, unsigned int count);
void sys_exit(int status);
int sys_fork();
END_C

# Check kernel configuration
system("perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS}'") == 0
    or do {
        sys_write(1, "Required features disabled\n", 24);
        sys_exit(1);
    };

# Simulate sensor data
my $samples = 5;
for my $i (1..$samples) {
    my $pid = sys_fork();
    if ($pid == -1) {
        sys_write(1, "Fork failed\n", 12);
        sys_exit(1);
    }
    if ($pid == 0) {
        # Child: simulate sensor reading
        my $temp = int(rand(30) + 20); # Simulated temperature (20-50)
        my $msg = "Sensor Temp: $temp C\n";
        sys_write(1, $msg, length($msg));
        sys_exit(0);
    }
}

# Parent: report completion
my $msg = "Sensor Sampling Done: $samples samples\n";
sys_write(1, $msg, length($msg));
sys_exit(0);
