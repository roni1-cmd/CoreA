#!/usr/bin/perl
# CoreA System Call Interface for Perl
# Provides Perl bindings for system calls

package CoreA::Syscall;

use strict;
use warnings;
use Inline C => Config => MYEXTLIB => '-lc';

# System call numbers
use constant SYS_WRITE => 1;
use constant SYS_EXIT  => 2;
use constant SYS_FORK  => 3;
use constant SYS_PIPE  => 4;

# Load kernel configuration to check enabled features
my $config = do 'config/kernel.conf' or die "Cannot load kernel.conf: $!";

# Export system call functions
use Exporter 'import';
our @EXPORT = qw(sys_write sys_exit sys_fork sys_pipe);

# Inline C for system call invocation
use Inline C => <<'END_C';
#include <stdint.h>

/* Generic system call wrapper */
int64_t syscall(int64_t num, int64_t a1, int64_t a2, int64_t a3) {
    int64_t ret;
    asm volatile ("int $0x80"
                  : "=a"(ret)
                  : "a"(num), "b"(a1), "c"(a2), "d"(a3)
                  : "memory");
    return ret;
}

/* C wrappers for system calls */
int sys_write(int fd, const char *buf, size_t count) {
    return (int)syscall(SYS_WRITE, fd, (int64_t)buf, (int64_t)count);
}

void sys_exit(int status) {
    syscall(SYS_EXIT, status, 0, 0);
}

int sys_fork() {
    return (int)syscall(SYS_FORK, 0, 0, 0);
}

int sys_pipe(int *pipefd) {
    return (int)syscall(SYS_PIPE, (int64_t)pipefd, 0, 0);
}
END_C

# Perl wrappers for system calls
sub sys_write {
    my ($fd, $buf, $count) = @_;
    return sys_write_c($fd, $buf, $count);
}

sub sys_exit {
    my ($status) = @_;
    sys_exit_c($status);
}

sub sys_fork {
    return sys_fork_c() if $config->{PROCESS} && $config->{SCHEDULER};
    die "sys_fork not supported: process management disabled";
}

sub sys_pipe {
    my ($pipefd_ref) = @_;
    die "sys_pipe not supported: IPC_PIPE disabled" unless $config->{IPC_PIPE};
    my @pipefd = (0, 0);
    my $ret = sys_pipe_c(\@pipefd);
    @$pipefd_ref = @pipefd if $ret == 0;
    return $ret;
}

1;
