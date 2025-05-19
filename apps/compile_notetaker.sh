#!/bin/bash

# Compile notetaker.c to an object file
gcc -m32 -ffreestanding -c notetaker.c -o notetaker.o -I../src/kernel/

# Link the object file to a binary, setting the entry point at 0x200000
ld -m elf_i386 -Ttext 0x200000 -e notetaker_init notetaker.o -o notetaker.bin

# Convert the binary to a flat image for Corea
objcopy -O binary notetaker.bin notetaker.img

# Clean up intermediate files
rm -f notetaker.o notetaker.bin
