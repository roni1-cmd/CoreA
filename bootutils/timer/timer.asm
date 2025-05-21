; CoreA Boot Timer (Assembly)
; Measures boot time via cycle counts, reports via sys_write

section .data
    conf_cmd db "perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS}'", 0
    conf_err db "Required features disabled", 10, 0
    conf_err_len equ $ - conf_err
    time_msg db "Boot Time: ", 0
    time_msg_len equ $ - time_msg
    time_suffix db " cycles", 10, 0
    time_suffix_len equ $ - time_suffix

section .bss
    cycles resd 1
    buffer resb 32

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
    ; Simulate cycle count (replace with RDTSC for real hardware)
    mov dword [cycles], 1000000 ; Simulated 1M cycles

    ; Convert cycles to string
    mov eax, [cycles]
    mov edi, buffer
    call int_to_string

    ; Output message
    mov eax, 1
    mov ebx, time_msg
    mov edx, time_msg_len
    call sys_write

    ; Output cycles
    mov eax, 1
    mov ebx, buffer
    mov edx, edi
    sub edx, buffer
    call sys_write

    ; Output suffix
    mov eax, 1
    mov ebx, time_suffix
    mov edx, time_suffix_len
    call sys_write

    ; Exit
    mov eax, 0
    call sys_exit

; Convert integer to string
int_to_string:
    mov ecx, 10
    mov esi, 0
.convert_loop:
    xor edx, edx
    div ecx
    add dl, '0'
    mov [edi + esi], dl
    inc esi
    test eax, eax
    jnz .convert_loop
    mov ecx, esi
    shr ecx, 1
    mov ebx, 0
.reverse_loop:
    mov al, [edi + ebx]
    mov dl, [edi + esi - 1]
    mov [edi + ebx], dl
    mov [edi + esi - 1], al
    inc ebx
    dec esi
    loop .reverse_loop
    mov edi, buffer
    add edi, esi
    ret
