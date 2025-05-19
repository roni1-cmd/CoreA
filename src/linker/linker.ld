ENTRY(kmain)
SECTIONS {
    . = 0x10000;
    .text : { *(.text) }
    .data : { *(.data) }
    .bss  : { *(.bss)  }
    . = 0x20000;
    .user : { *(shell.o) *(ramfs.o) *(task.o) }
}
