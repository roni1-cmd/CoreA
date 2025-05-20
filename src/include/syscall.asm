; CoreA System Call Macros
; Defines macros for invoking system calls in Assembly

; System call numbers
%define SYS_WRITE 1
%define SYS_EXIT  2
%define SYS_FORK  3
%define SYS_PIPE  4

; Macro for system call with 0 arguments
%macro SYSCALL0 1
    mov rax, %1
    int 0x80
%endmacro

; Macro for system call with 1 argument
%macro SYSCALL1 2
    mov rax, %1
    mov rbx, %2
    int 0x80
%endmacro

; Macro for system call with 2 arguments
%macro SYSCALL2 3
    mov rax, %1
    mov rbx, %2
    mov rcx, %3
    int 0x80
%endmacro

; Macro for system call with 3 arguments
%macro SYSCALL3 4
    mov rax, %1
    mov rbx, %2
    mov rcx, %3
    mov rdx, %4
    int 0x80
%endmacro

; Example usage:
; SYSCALL1 SYS_WRITE, rdi, rsi, rdx  ; sys_write(fd, buf, count)
; SYSCALL1 SYS_EXIT, rdi             ; sys_exit(status)
; SYSCALL0 SYS_FORK                  ; sys_fork()
; SYSCALL1 SYS_PIPE, rdi             ; sys_pipe(pipefd)
