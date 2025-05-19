ENTRY(kmain)
SECTIONS {
    . = 0x10000;
    .text : { *(.text) }
    .data : { *(.data) }
    .bss  : { *(.bss)  }
    . = 0x20000;
    .user : { *(shell.o) *(ramfs.o) *(task.o) *(disk.o) *(fs.o) *(paging.o) *(net.o) *(rtc.o) *(dynlink.o) *(usb.o) *(gui.o) *(thread.o) *(mouse.o) *(ipc.o) *(acpi.o) *(audio.o) *(vfs.o) *(profiler.o) }
}
