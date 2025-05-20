; CoreA IPC Benchmark (Assembly)
; Tests pipe throughput

%include "syscall.asm"
section .text
global _start
_start:
    mov rdi, pipefd
    SYSCALL1 SYS_PIPE, rdi
    cmp rax, 0
    jne error
    mov rcx, 1000     ; iterations
    mov rsi, data
    mov rdx, 1024     ; 1KB
loop_start:
    mov rdi, [pipefd+8]
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx
    loop loop_start
    mov rdi, 1
    mov rsi, msg
    mov rdx, 17
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx
    mov rdi, 0
    SYSCALL1 SYS_EXIT, rdi
error:
    mov rdi, 1
    mov rsi, err_msg
    mov rdx, 22
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx
    mov rdi, 1
    SYSCALL1 SYS_EXIT, rdi
section .data
data: times 1024 db 'x'
pipefd: times 2 dd 0
msg: db "IPC Benchmark: Done", 10
err_msg: db "IPC Benchmark: Failed", 10
