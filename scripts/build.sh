#!/bin/bash
nasm -f bin src/boot/boot.asm -o boot.bin
i686-elf-gcc -ffreestanding -c src/kernel/kernel.c -o kernel.o
i686-elf-gcc -ffreestanding -c src/kernel/shell.c -o shell.o
i686-elf-ld -T src/linker/linker.ld kernel.o shell.o -o kernel.bin --oformat binary
cat boot.bin kernel.bin > os.bin
dd if=/dev/zero of=disk.img bs=512 count=2880
dd if=os.bin of=disk.img conv=notrunc
mkdir -p iso/boot
cp disk.img iso/boot/
genisoimage -b boot/disk.img -o os.iso iso
rm -rf *.bin *.o iso
echo "Build complete! Run with: qemu-system-i386 -cdrom os.iso"
