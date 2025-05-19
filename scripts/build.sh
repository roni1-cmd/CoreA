#!/bin/bash
nasm -f elf32 src/boot/boot.asm -o src/boot/boot.o
gcc -m32 -ffreestanding -c src/kernel/kernel.c -o src/kernel/kernel.o
gcc -m32 -ffreestanding -c src/kernel/shell.c -o src/kernel/shell.o
gcc -m32 -ffreestanding -c src/kernel/ramfs.c -o src/kernel/ramfs.o
gcc -m32 -ffreestanding -c src/kernel/task.c -o src/kernel/task.o
gcc -m32 -ffreestanding -c src/kernel/disk.c -o src/kernel/disk.o
gcc -m32 -ffreestanding -c src/kernel/fs.c -o src/kernel/fs.o
gcc -m32 -ffreestanding -c src/kernel/paging.c -o src/kernel/paging.o
gcc -m32 -ffreestanding -c src/kernel/net.c -o src/kernel/net.o
gcc -m32 -ffreestanding -c src/kernel/rtc.c -o src/kernel/rtc.o
gcc -m32 -ffreestanding -c src/kernel/dynlink.c -o src/kernel/dynlink.o
gcc -m32 -ffreestanding -c src/kernel/usb.c -o src/kernel/usb.o
gcc -m32 -ffreestanding -c src/kernel/gui.c -o src/kernel/gui.o
gcc -m32 -ffreestanding -c src/kernel/thread.c -o src/kernel/thread.o
gcc -m32 -ffreestanding -c src/kernel/mouse.c -o src/kernel/mouse.o
gcc -m32 -ffreestanding -c src/kernel/ipc.c -o src/kernel/ipc.o
gcc -m32 -ffreestanding -c src/kernel/acpi.c -o src/kernel/acpi.o
gcc -m32 -ffreestanding -c src/kernel/audio.c -o src/kernel/audio.o
gcc -m32 -ffreestanding -c src/kernel/vfs.c -o src/kernel/vfs.o
gcc -m32 -ffreestanding -c src/kernel/profiler.c -o src/kernel/profiler.o
gcc -m32 -ffreestanding -c src/kernel/font.c -o src/kernel/font.o
gcc -m32 -ffreestanding -c src/kernel/slab.c -o src/kernel/slab.o
gcc -m32 -ffreestanding -c src/kernel/tcp.c -o src/kernel/tcp.o
gcc -m32 -ffreestanding -c src/kernel/p2p.c -o src/kernel/p2p.o
gcc -m32 -ffreestanding -c src/kernel/file_explorer.c -o src/kernel/file_explorer.o
gcc -m32 -ffreestanding -c src/kernel/crypto.c -o src/kernel/crypto.o
gcc -m32 -ffreestanding -c src/kernel/keymgmt.c -o src/kernel/keymgmt.o
gcc -m32 -ffreestanding -c src/kernel/taskmgr.c -o src/kernel/taskmgr.o
gcc -m32 -ffreestanding -c src/kernel/netbrowser.c -o src/kernel/netbrowser.o
gcc -m32 -ffreestanding -c src/kernel/clipboard.c -o src/kernel/clipboard.o
ld -m elf_i386 -T src/linker/linker.ld src/boot/boot.o src/kernel/kernel.o src/kernel/shell.o src/kernel/ramfs.o src/kernel/task.o src/kernel/disk.o src/kernel/fs.o src/kernel/paging.o src/kernel/net.o src/kernel/rtc.o src/kernel/dynlink.o src/kernel/usb.o src/kernel/gui.o src/kernel/thread.o src/kernel/mouse.o src/kernel/ipc.o src/kernel/acpi.o src/kernel/audio.o src/kernel/vfs.o src/kernel/profiler.o src/kernel/font.o src/kernel/slab.o src/kernel/tcp.o src/kernel/p2p.o src/kernel/file_explorer.o src/kernel/crypto.o src/kernel/keymgmt.o src/kernel/taskmgr.o src/kernel/netbrowser.o src/kernel/clipboard.o -o minios.bin
objcopy -O binary minios.bin corea.img
