; CoreA CPU Feature Detection Module
; Detects various x86/x86_64 CPU features using CPUID instruction
; For use in determining available CPU capabilities during kernel initialization

[BITS 64]                       ; 64-bit code

section .data
    ; Feature flags storage
    align 16
    cpu_features:
        .vendor_id      times 13 db 0   ; CPU vendor string (null-terminated)
        .family         dd 0            ; CPU family
        .model          dd 0            ; CPU model
        .stepping       dd 0            ; CPU stepping
        .features_ecx   dd 0            ; Standard features (ECX)
        .features_edx   dd 0            ; Standard features (EDX)
        .ext_features_ecx dd 0          ; Extended features (ECX)
        .ext_features_edx dd 0          ; Extended features (EDX)
        .brand_string   times 49 db 0   ; Full brand string (null-terminated)
        .max_cpuid      dd 0            ; Max supported CPUID level
        .max_ext_cpuid  dd 0            ; Max supported extended CPUID
        .cache_line     dd 0            ; Cache line size
        .cores          dd 0            ; Number of cores

section .text
    global detect_cpu_features     ; Export function symbol
    global get_cpu_vendor          ; Get CPU vendor string
    global cpu_has_feature         ; Check if specific feature is available
    global get_cpu_cores           ; Get number of CPU cores
    global get_cpu_brand           ; Get CPU brand string

; Main CPU detection function
; void detect_cpu_features(void);
detect_cpu_features:
    push rbp
    mov rbp, rsp
    push rbx                      ; CPUID clobbers rbx
    push rcx
    push rdx
    
    ; Get maximum CPUID level and vendor ID
    xor eax, eax                  ; EAX = 0: Get vendor ID
    cpuid
    
    ; Store max CPUID level
    mov [cpu_features.max_cpuid], eax
    
    ; Store vendor ID (12 bytes: EBX+EDX+ECX)
    mov [cpu_features.vendor_id], ebx
    mov [cpu_features.vendor_id + 4], edx
    mov [cpu_features.vendor_id + 8], ecx
    
    ; Get processor info and feature flags
    mov eax, 1                    ; EAX = 1: Get processor info
    cpuid
    
    ; Extract family, model, stepping
    mov ebx, eax
    and eax, 0x0F                 ; Stepping = bits 0-3
    mov [cpu_features.stepping], eax
    
    mov eax, ebx
    shr eax, 4
    and eax, 0x0F                 ; Model = bits 4-7
    mov [cpu_features.model], eax
    
    mov eax, ebx
    shr eax, 8
    and eax, 0x0F                 ; Family = bits 8-11
    mov [cpu_features.family], eax
    
    ; Store feature flags
    mov [cpu_features.features_ecx], ecx
    mov [cpu_features.features_edx], edx
    
    ; Check if extended CPUID is available
    mov eax, 0x80000000
    cpuid
    mov [cpu_features.max_ext_cpuid], eax
    
    ; Get extended features if available
    cmp eax, 0x80000001
    jb .no_ext_features
    
    mov eax, 0x80000001
    cpuid
    mov [cpu_features.ext_features_ecx], ecx
    mov [cpu_features.ext_features_edx], edx
    
.no_ext_features:
    ; Get processor brand string if available
    cmp dword [cpu_features.max_ext_cpuid], 0x80000004
    jb .no_brand_string
    
    ; Get brand string (needs three CPUID calls)
    mov eax, 0x80000002          ; First part of brand string
    cpuid
    mov [cpu_features.brand_string], eax
    mov [cpu_features.brand_string + 4], ebx
    mov [cpu_features.brand_string + 8], ecx
    mov [cpu_features.brand_string + 12], edx
    
    mov eax, 0x80000003          ; Second part of brand string
    cpuid
    mov [cpu_features.brand_string + 16], eax
    mov [cpu_features.brand_string + 20], ebx
    mov [cpu_features.brand_string + 24], ecx
    mov [cpu_features.brand_string + 28], edx
    
    mov eax, 0x80000004          ; Third part of brand string
    cpuid
    mov [cpu_features.brand_string + 32], eax
    mov [cpu_features.brand_string + 36], ebx
    mov [cpu_features.brand_string + 40], ecx
    mov [cpu_features.brand_string + 44], edx
    
.no_brand_string:
    ; Get cache line size if supported
    cmp dword [cpu_features.max_cpuid], 2
    jb .no_cache_info
    
    mov eax, 2                   ; Get cache info
    cpuid
    
    ; AL contains number of times to call CPUID
    ; For most modern CPUs, this is 1
    shr ebx, 8                   ; Get byte 1
    and ebx, 0xFF
    mov [cpu_features.cache_line], ebx
    
.no_cache_info:
    ; Get core count if supported
    cmp dword [cpu_features.max_cpuid], 4
    jb .default_cores
    
    mov eax, 4                   ; Get core info
    xor ecx, ecx                 ; ECX = 0, first cache level
    cpuid
    
    shr eax, 26                  ; EAX[31:26] + 1 is core count
    inc eax
    mov [cpu_features.cores], eax
    jmp .done
    
.default_cores:
    mov dword [cpu_features.cores], 1
    
.done:
    pop rdx
    pop rcx
    pop rbx
    pop rbp
    ret

; Check if CPU has a specific feature
; bool cpu_has_feature(int feature_id);
; Returns 1 if CPU has feature, 0 otherwise
; feature_id format: 0xXY where X is register (0-3) and Y is bit number (0-31)
; Registers: 0=ECX-STD, 1=EDX-STD, 2=ECX-EXT, 3=EDX-EXT
cpu_has_feature:
    push rbp
    mov rbp, rsp
    push rbx
    
    mov eax, edi                ; Get feature_id from first parameter
    mov ecx, eax
    shr ecx, 8                  ; Get register (high byte)
    and eax, 0xFF               ; Get bit number (low byte)
    
    ; Calculate bitmask: 1 << bit_number
    mov rdx, 1
    mov cl, al
    shl rdx, cl
    
    ; Select appropriate register based on register code
    mov ebx, ecx
    shr ebx, 8
    cmp ebx, 0
    je .check_std_ecx
    cmp ebx, 1
    je .check_std_edx
    cmp ebx, 2
    je .check_ext_ecx
    cmp ebx, 3
    je .check_ext_edx
    jmp .not_supported
    
.check_std_ecx:
    mov rax, [cpu_features.features_ecx]
    and rax, rdx
    jnz .supported
    jmp .not_supported
    
.check_std_edx:
    mov rax, [cpu_features.features_edx]
    and rax, rdx
    jnz .supported
    jmp .not_supported
    
.check_ext_ecx:
    mov rax, [cpu_features.ext_features_ecx]
    and rax, rdx
    jnz .supported
    jmp .not_supported
    
.check_ext_edx:
    mov rax, [cpu_features.ext_features_edx]
    and rax, rdx
    jnz .supported
    jmp .not_supported
    
.supported:
    mov eax, 1
    jmp .done
    
.not_supported:
    mov eax, 0
    
.done:
    pop rbx
    pop rbp
    ret

; Get CPU vendor string
; const char* get_cpu_vendor(void);
get_cpu_vendor:
    mov rax, cpu_features.vendor_id
    ret
    
; Get CPU brand string
; const char* get_cpu_brand(void);
get_cpu_brand:
    mov rax, cpu_features.brand_string
    ret
    
; Get number of CPU cores
; int get_cpu_cores(void);
get_cpu_cores:
    mov eax, [cpu_features.cores]
    ret

; CPU Feature Bit Constants
; Use these constants with cpu_has_feature()
; Format: CPUFEATURE_NAME equ 0xXY
; where X is register (0-3) and Y is bit position (0-31)

; Standard Features (ECX)
CPUFEATURE_SSE3        equ 0x000    ; SSE3 Instructions
CPUFEATURE_PCLMULQDQ   equ 0x001    ; PCLMULQDQ Instruction
CPUFEATURE_DTES64      equ 0x002    ; 64-bit Debug Store
CPUFEATURE_MONITOR     equ 0x003    ; MONITOR/MWAIT instructions
CPUFEATURE_DS_CPL      equ 0x004    ; CPL Qualified Debug Store
CPUFEATURE_VMX         equ 0x005    ; Virtual Machine Extensions
CPUFEATURE_SMX         equ 0x006    ; Safer Mode Extensions
CPUFEATURE_EST         equ 0x007    ; Enhanced SpeedStep
CPUFEATURE_TM2         equ 0x008    ; Thermal Monitor 2
CPUFEATURE_SSSE3       equ 0x009    ; SSSE3 Instructions
CPUFEATURE_CNXT_ID     equ 0x00A    ; L1 Context ID
CPUFEATURE_FMA         equ 0x00C    ; Fused Multiply Add
CPUFEATURE_CMPXCHG16B  equ 0x00D    ; CMPXCHG16B Instruction
CPUFEATURE_XTPR        equ 0x00E    ; Send Task Priority Messages
CPUFEATURE_PDCM        equ 0x00F    ; Perf/Debug Capability MSR
CPUFEATURE_PCID        equ 0x011    ; Process-context Identifiers
CPUFEATURE_DCA         equ 0x012    ; Direct Cache Access
CPUFEATURE_SSE41       equ 0x013    ; SSE4.1 Instructions
CPUFEATURE_SSE42       equ 0x014    ; SSE4.2 Instructions
CPUFEATURE_X2APIC      equ 0x015    ; x2APIC Support
CPUFEATURE_MOVBE       equ 0x016    ; MOVBE Instruction
CPUFEATURE_POPCNT      equ 0x017    ; POPCNT Instruction
CPUFEATURE_TSC_DEADLINE equ 0x018   ; TSC Deadline
CPUFEATURE_AES         equ 0x019    ; AES Instructions
CPUFEATURE_XSAVE       equ 0x01A    ; XSAVE/XRSTOR
CPUFEATURE_OSXSAVE     equ 0x01B    ; XSAVE Enabled by OS
CPUFEATURE_AVX         equ 0x01C    ; Advanced Vector Extensions
CPUFEATURE_F16C        equ 0x01D    ; 16-bit FP Conversion
CPUFEATURE_RDRAND      equ 0x01E    ; RDRAND Instruction

; Standard Features (EDX)
CPUFEATURE_FPU         equ 0x100    ; Floating Point Unit
CPUFEATURE_VME         equ 0x101    ; Virtual 8086 Mode Enhancements
CPUFEATURE_DE          equ 0x102    ; Debugging Extensions
CPUFEATURE_PSE         equ 0x103    ; Page Size Extension
CPUFEATURE_TSC         equ 0x104    ; Time Stamp Counter
CPUFEATURE_MSR         equ 0x105    ; Model Specific Registers
CPUFEATURE_PAE         equ 0x106    ; Physical Address Extension
CPUFEATURE_MCE         equ 0x107    ; Machine Check Exception
CPUFEATURE_CX8         equ 0x108    ; CMPXCHG8B Instruction
CPUFEATURE_APIC        equ 0x109    ; APIC On-Chip
CPUFEATURE_SEP         equ 0x10B    ; SYSENTER/SYSEXIT
CPUFEATURE_MTRR        equ 0x10C    ; Memory Type Range Registers
CPUFEATURE_PGE         equ 0x10D    ; Page Global Enable
CPUFEATURE_MCA         equ 0x10E    ; Machine Check Architecture
CPUFEATURE_CMOV        equ 0x10F    ; Conditional Move Instructions
CPUFEATURE_PAT         equ 0x110    ; Page Attribute Table
CPUFEATURE_PSE36       equ 0x111    ; 36-bit Page Size Extension
CPUFEATURE_PSN         equ 0x112    ; Processor Serial Number
CPUFEATURE_CLFLUSH     equ 0x113    ; CLFLUSH Instruction
CPUFEATURE_DS          equ 0x115    ; Debug Store
CPUFEATURE_ACPI        equ 0x116    ; Thermal Monitor and Clock Control
CPUFEATURE_MMX         equ 0x117    ; MMX Technology
CPUFEATURE_FXSR        equ 0x118    ; FXSAVE and FXRSTOR Instructions
CPUFEATURE_SSE         equ 0x119    ; SSE Instructions
CPUFEATURE_SSE2        equ 0x11A    ; SSE2 Instructions
CPUFEATURE_SS          equ 0x11B    ; Self Snoop
CPUFEATURE_HTT         equ 0x11C    ; Multi-threading
CPUFEATURE_TM          equ 0x11D    ; Thermal Monitor
CPUFEATURE_PBE         equ 0x11F    ; Pending Break Enable

; Extended Features (ECX)
CPUFEATURE_LAHF        equ 0x200    ; LAHF/SAHF Instructions
CPUFEATURE_PREFETCHW   equ 0x208    ; PREFETCHW Instruction

; Extended Features (EDX)
CPUFEATURE_SYSCALL     equ 0x300    ; SYSCALL/SYSRET
CPUFEATURE_NX          equ 0x314    ; Execute Disable Bit
CPUFEATURE_PDPE1GB     equ 0x31A    ; 1GB Pages
CPUFEATURE_RDTSCP      equ 0x31B    ; RDTSCP Instruction
CPUFEATURE_LM          equ 0x31D    ; 64-bit Long Mode
