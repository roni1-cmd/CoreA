#!/bin/bash

# Compile backup.c to an object file
gcc -m32 -ffreestanding -c backup.c -o backup.o -I../src/kernel/

# Link the object file to a binary, setting the entry point at 0x200000
ld -m elf_i386 -Ttext 0x200000 -e backup_init backup.o -o backup.bin

# Convert the binary to a flat image for Corea
objcopy -O binary backup.bin backup.img

# Clean up intermediate files
rm -f backup.o backup.bin
