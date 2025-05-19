#!/bin/bash

# Compile sysmon.c to an object file
gcc -m32 -ffreestanding -c sysmon.c -o sysmon.o -I../src/kernel/

# Link the object file to a binary, setting the entry point at 0x200000
ld -m elf_i386 -Ttext 0x200000 -e sysmon_init sysmon.o -o sysmon.bin

# Convert the binary to a flat image for Corea
objcopy -O binary sysmon.bin sysmon.img

# Clean up intermediate files
rm -f sysmon.o sysmon.bin
