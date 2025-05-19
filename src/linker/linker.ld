#!/bin/bash
nasm -f bin src/boot/boot.asm -o boot.bin
i686-elf-gcc -ffreestanding -c src/kernel/kernel.c -o kernel.o
i686-elf-gcc -ffreestanding -c src/kernel/shell.c -o shell.o
i686-elf-gcc -ffreestanding -c src/kernel/ramfs.c -o ramfs.o
i686-elf-gcc -ffreestanding -c src/kernel/task.c -o task.o
i686-elf-gcc -ffreestanding -c src/kernel/disk.c -o disk.o
i686-elf-gcc -ffreestanding -c src/kernel/fs.c -o fs.o
i686-elf-gcc -ffreestanding -c src/kernel/paging.c -o paging.o
i686-elf-gcc -ffreestanding -c src/kernel/net.c -o net.o
i686-elf-gcc -ffreestanding -c src/kernel/rtc.c -o rtc.o
i686-elf-gcc -ffreestanding -c src/kernel/dynlink.c -o dynlink.o
i686-elf-gcc -ffreestanding -c src/kernel/usb.c -o usb.o
i686-elf-gcc -ffreestanding -c src/kernel/gui.c -o gui.o
i686-elf-gcc -ffreestanding -c src/kernel/thread.c -o thread.o
i686-elf-gcc -ffreestanding -c src/kernel/mouse.c -o mouse.o
i686-elf-gcc -ffreestanding -c src/kernel/ipc.c -o ipc.o
i686-elf-gcc -ffreestanding -c src/kernel/acpi.c -o acpi.o
i686-elf-gcc -ffreestanding -c src/kernel/audio.c -o audio.o
i686-elf-gcc -ffreestanding -c src/kernel/vfs.c -o vfs.o
i686-elf-gcc -ffreestanding -c src/kernel/profiler.c -o profiler.o
i686-elf-gcc -ffreestanding -c src/kernel/font.c -o font.o
i686-elf-ld -T src/linker/linker.ld kernel.o shell.o ramfs.o task.o disk.o fs.o paging.o net.o rtc.o dynlink.o usb.o gui.o thread.o mouse.o ipc.o acpi.o audio.o vfs.o profiler.o font.o -o kernel.bin --oformat binary
cat boot.bin kernel.bin > os.bin
dd if=/dev/zero of=disk.img bs=512 count=2880
dd if=os.bin of=disk.img conv=notrunc
mkdir -p iso/boot
cp disk.img iso/boot/
genisoimage -b boot/disk.img -o os.iso iso
rm -rf *.bin *.o iso
echo "Build complete! Run with: qemu-system-i386 -cdrom os.iso -netdev user,id=net0 -device rtl8139,netdev=net0"
