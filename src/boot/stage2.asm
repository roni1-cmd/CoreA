; CoreA OS Stage 2 Bootloader
; Switches to protected mode and loads kernel

[BITS 16]
[ORG 0x7E00]

stage2_start:
    ; Display stage 2 message
    mov si, stage2_msg
    call print_string_16
    
    ; Enable A20 line
    call enable_a20
    
    ; Load kernel from disk
    call load_kernel
    
    ; Switch to protected mode
    cli                 ; Disable interrupts
    lgdt [gdt_descriptor]
    
    ; Set PE bit in CR0
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    
    ; Far jump to protected mode
    jmp CODE_SEG:protected_mode_start

[BITS 32]
protected_mode_start:
    ; Set up protected mode segments
    mov ax, DATA_SEG
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    ; Set up stack
    mov esp, 0x90000
    
    ; Display protected mode message
    mov esi, pmode_msg
    call print_string_32
    
    ; Jump to kernel
    jmp 0x100000

[BITS 16]
enable_a20:
    ; Fast A20 gate method
    in al, 0x92
    or al, 2
    out 0x92, al
    ret

load_kernel:
    ; Load kernel at 0x100000 (1MB)
    ; This is a simplified version - real implementation would be more complex
    mov ah, 0x02        ; Read sectors
    mov al, 64          ; Number of sectors (32KB kernel)
    mov ch, 0           ; Cylinder 0
    mov cl, 6           ; Start from sector 6
    mov dh, 0           ; Head 0
    mov dl, [0x7C00 + 510 - 1]  ; Boot drive
    mov bx, 0x8000      ; Temporary load address
    int 0x13
    
    ; Copy to 1MB (this requires more complex code in reality)
    ret

print_string_16:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0E
    mov bh, 0
    mov bl, 7
    int 0x10
    jmp print_string_16
.done:
    ret

[BITS 32]
print_string_32:
    pusha
    mov edx, 0xB8000    ; VGA text buffer
.loop:
    lodsb
    or al, al
    jz .done
    mov [edx], al
    mov byte [edx + 1], 0x07
    add edx, 2
    jmp .loop
.done:
    popa
    ret

; GDT (Global Descriptor Table)
gdt_start:
    ; Null descriptor
    dd 0x0
    dd 0x0

    ; Code segment descriptor
    dw 0xFFFF       ; Limit (bits 0-15)
    dw 0x0          ; Base (bits 0-15)
    db 0x0          ; Base (bits 16-23)
    db 10011010b    ; Access byte
    db 11001111b    ; Flags + Limit (bits 16-19)
    db 0x0          ; Base (bits 24-31)

    ; Data segment descriptor
    dw 0xFFFF       ; Limit (bits 0-15)
    dw 0x0          ; Base (bits 0-15)
    db 0x0          ; Base (bits 16-23)
    db 10010010b    ; Access byte
    db 11001111b    ; Flags + Limit (bits 16-19)
    db 0x0          ; Base (bits 24-31)

gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1
    dd gdt_start

; Segment selectors
CODE_SEG equ gdt_start + 0x8
DATA_SEG equ gdt_start + 0x10

; Messages
stage2_msg db 'Stage 2 Bootloader Loaded', 0x0D, 0x0A, 0
pmode_msg db 'Protected Mode Enabled', 0

; Pad to sector boundary
times 2048-($-$$) db 0
