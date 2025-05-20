CoreA System Call API
This document describes the system call interface for the CoreA operating system, providing access to kernel services such as file I/O, process management, and inter-process communication (IPC). The API is accessible from multiple languages, including C, Perl, and Assembly.
System Calls
sys_write
Write data to a file descriptor.

C:
int sys_write(int fd, const void *buf, size_t count);


fd: File descriptor (e.g., 1 for stdout).
buf: Pointer to data buffer.
count: Number of bytes to write.
Returns: Number of bytes written, or -1 on error.


Perl:
use CoreA::Syscall;
my $bytes_written = sys_write($fd, $buffer, $count);


$fd: File descriptor.
$buffer: String or buffer to write.
$count: Length of data.
Returns: Number of bytes written, or dies on error.


Assembly:
SYSCALL3 SYS_WRITE, rdi, rsi, rdx


rdi: File descriptor.
rsi: Buffer address.
rdx: Byte count.
Returns: Bytes written in rax.



sys_exit
Terminate the calling process.

C:
void sys_exit(int status);


status: Exit status code.
Does not return.


Perl:
use CoreA::Syscall;
sys_exit($status);


$status: Exit status code.
Does not return.


Assembly:
SYSCALL1 SYS_EXIT, rdi


rdi: Exit status.
Does not return.



sys_fork
Create a new process.

C:
int sys_fork(void);


Returns: 0 in child, child PID in parent, or -1 on error.
Requires: CONFIG_PROCESS and CONFIG_SCHEDULER enabled in kernel.conf.


Perl:
use CoreA::Syscall;
my $pid = sys_fork();


Returns: 0 in child, child PID in parent, or dies if not supported.


Assembly:
SYSCALL0 SYS_FORK


Returns: 0 in child, child PID in parent in rax.



sys_pipe
Create a pipe for inter-process communication.

C:
int sys_pipe(int pipefd[2]);


pipefd: Array to store read and write file descriptors.
Returns: 0 on success, -1 on error.
Requires: CONFIG_IPC_PIPE enabled in kernel.conf.


Perl:
use CoreA::Syscall;
my @pipefd;
sys_pipe(\@pipefd);


@pipefd: Array to store read and write file descriptors.
Returns: 0 on success, or dies if not supported or on error.


Assembly:
SYSCALL1 SYS_PIPE, rdi


rdi: Address of pipefd array.
Returns: 0 in rax on success.



Usage Notes

C: Include <kernel/syscall.h> and use the provided prototypes. System calls are invoked via the SYSCALL macro, which triggers interrupt 0x80.
Perl: Use the CoreA::Syscall module from src/userspace/lib/CoreA/Syscall.pm. The module checks config/kernel.conf to ensure features are enabled.
Assembly: Include syscall.asm and use the SYSCALL0, SYSCALL1, SYSCALL2, or SYSCALL3 macros based on the number of arguments.
Dependencies: Some system calls (e.g., sys_fork, sys_pipe) depend on kernel features enabled in config/kernel.conf. Check the configuration before use.

Example
C
#include <kernel/syscall.h>
#include <stdint.h>

int main() {
    const char *msg = "Hello, CoreA!\n";
    sys_write(1, msg, 14);
    sys_exit(0);
    return 0;
}

Perl
use CoreA::Syscall;

sys_write(1, "Hello, CoreA!\n", 14);
sys_exit(0);

Assembly
%include "syscall.asm"
section .text
global _start
_start:
    mov rdi, 1
    mov rsi, msg
    mov rdx, 14
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx
    mov rdi, 0
    SYSCALL1 SYS_EXIT, rdi
section .data
msg: db "Hello, CoreA!", 10

