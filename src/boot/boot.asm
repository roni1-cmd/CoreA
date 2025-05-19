[org 0x7c00]
[bits 16]

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00
    mov si, boot_msg
    call print_string
    mov ah, 0x02
    mov al, 10
    mov ch, 0
    mov cl, 2
    mov dh, 0
    mov bx, 0x1000
    mov es, bx
    xor bx, bx
    int 0x13
    jc disk_error
    lgdt [gdt_desc]
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    jmp 0x08:protected_mode
disk_error:
    mov si, error_msg
    call print_string
    jmp $
print_string:
    lodsb
    cmp al, 0
    je .done
    mov ah, 0x0e
    int 0x10
    jmp print_string
.done:
    ret
[bits 32]
protected_mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x90000
    call 0x10000
    cli
    hlt
gdt_start:
    dq 0
gdt_code_kernel:
    dw 0xffff
    dw 0
    db 0
    db 0x9a
    db 0xcf
    db 0
gdt_data_kernel:
    dw 0xffff
    dw 0
    db 0
    db 0x92
    db 0xcf
    db 0
gdt_code_user:
    dw 0xffff
    dw 0
    db 0
    db 0x9a
    db 0xc0
    db 0
gdt_data_user:
    dw 0xffff
    dw 0
    db 0
    db 0x92
    db 0xc0
    db 0
gdt_end:
gdt_desc:
    dw gdt_end - gdt_start - 1
    dd gdt_start
boot_msg: db "Booting...", 0
error_msg: db "Disk error!", 0
times 510-($-$$) db 0
dw 0xaa55
