; CoreA Buzzer Controller (Assembly)
; Activates simulated buzzer, reports via sys_write

section .data
    conf_cmd db "perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS}'", 0
    conf_err db "Required features disabled", 10, 0
    conf_err_len equ $ - conf_err
    buzz_msg db "Buzzer Active: ", 0
    buzz_msg_len equ $ - buzz_msg
    buzz_off db "Buzzer Off", 10, 0
    buzz_off_len equ $ - buzz_off

section .bss
    state resd 1

section .text
    global _start
    extern system
    extern sys_write
    extern sys_exit

_start:
    ; Check kernel configuration
    push conf_cmd
    call system
    add esp, 4
    cmp eax, 0
    je .config_ok
    mov eax, 1
    mov ebx, conf_err
    mov ecx, conf_err_len
    call sys_write
    mov eax, 1
    call sys_exit

.config_ok:
    ; Simulate buzzer activation
    mov ecx, 5
.buzz_loop:
    mov dword [state], 1
    mov eax, 1
    mov ebx, buzz_msg
    mov edx, buzz_msg_len
    call sys_write
    ; Simulate delay
    mov esi, 1000000
.delay:
    dec esi
    jnz .delay
    mov dword [state], 0
    mov eax, 1
    mov ebx, buzz_off
    mov edx, buzz_off_len
    call sys_write
    loop .buzz_loop

    ; Exit
    mov eax, 0
    call sys_exit
