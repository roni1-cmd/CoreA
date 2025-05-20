; CoreA Interrupt Handlers
; Low-level interrupt handlers in Assembly

section .text
global interrupt_handlers
extern interrupt_handler_c

; Macro to generate interrupt handler stubs
%macro INT_HANDLER 1
global int%1_handler
int%1_handler:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    mov rdi, %1       ; Pass interrupt number as first argument
    call interrupt_handler_c
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    iretq
%endmacro

; Define handlers for interrupts 0-31 (exceptions) and 0x80 (syscall)
INT_HANDLER 0   ; Divide by zero
INT_HANDLER 1   ; Debug
INT_HANDLER 13  ; General protection fault
INT_HANDLER 14  ; Page fault
INT_HANDLER 80  ; System call

; Array of handler addresses for IDT
section .data
interrupt_handlers:
%assign i 0
%rep 32
    dq int%+i_handler
%assign i i+1
%endrep
    dq int80_handler  ; System call at vector 0x80
