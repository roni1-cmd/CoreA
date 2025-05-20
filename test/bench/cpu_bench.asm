; CoreA CPU Benchmark (Assembly)
; Measures CPU performance with arithmetic loop

%include "syscall.asm"
section .text
global _start
_start:
    mov rcx, 1000000  ; iterations
    xor rbx, rbx      ; result = 0
loop_start:
    mov rax, rcx
    shl rax, 1        ; rax = rcx * 2
    add rbx, rax      ; result += i * 2
    loop loop_start
    ; Print result (simplified, assuming sys_write available)
    mov rdi, 1
    mov rsi, msg
    mov rdx, 20
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx
    mov rdi, 0
    SYSCALL1 SYS_EXIT, rdi
section .data
msg: db "CPU Benchmark Result", 10
