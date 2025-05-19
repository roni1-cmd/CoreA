ENTRY(boot_start)
SECTIONS {
    . = 0x7C00;
    .boot : {
        boot/boot.o
    }
    . = 0x100000;
    .text : {
        *(.text)
    }
    .data : {
        *(.data)
        *(.rodata)
    }
    .bss : {
        *(.bss)
    }
}
