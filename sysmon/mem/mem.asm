; CoreA Memory Usage Tracker (Assembly)
; Monitors memory allocation, reports via sys_write

section .data
    conf_cmd db "perl -e 'exit 1 unless do \"config/kernel.conf\"->{PROCESS}'", 0
    conf_err db "Required features disabled", 10, 0
    conf_err_len equ $ - conf_err
    mem_msg db "Memory Used: ", 0
    mem_msg_len equ $ - mem_msg
    mem_suffix db " bytes", 10, 0
    mem_suffix_len equ $ - mem_suffix

section .bss
    mem_used resd 1
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
    ; Simulate memory usage (replace with kernel memory query)
    mov dword [mem_used], 524288 ; Simulated 512KB

    ; Convert usage to string
    mov eax, [mem_used]
    mov edi, buffer
    call int_to_string

    ; Output message
    mov eax, 1
    mov ebx, mem_msg
    mov edx, mem_msg_len
    call sys_write

    ; Output usage
    mov eax, 1
    mov ebx, buffer
    mov edx, edi
    sub edx, buffer
    call sys_write

    ; Output suffix
    mov eax, 1
    mov ebx, mem_suffix
    mov edx, mem_suffix_len
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
