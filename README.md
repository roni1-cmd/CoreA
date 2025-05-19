# Corea Operating System

## Overview

Corea is a lightweight, 32-bit x86 operating system designed for educational and experimental purposes. Built from scratch, it provides a minimal yet functional environment with a focus on modularity and extensibility. Corea includes a custom bootloader, kernel, shell, and a variety of features such as a graphical user interface (GUI), networking, and file system support.

## Features

Bootloader: Custom bootloader for loading the kernel on x86 architecture.
Kernel: Supports multitasking, virtual memory, threading, and dynamic linking.
Shell: Command-line interface with scripting support for executing command sequences.

## File Systems:

Virtual File System (VFS) layer for abstraction.
RAMFS for in-memory file storage.
On-disk file system for persistent storage.
GUI: VGA-based 320x200, 256-color graphical interface with:


##Window manager supporting multiple windows.

Mouse support via PS/2 driver.
Text rendering with bitmap fonts.
Interactive buttons and event handling for mouse/keyboard input.
Networking: Basic RTL8139 driver for sending/receiving packets (e.g., ping support).
USB: Support for USB devices, including keyboard input.
Inter-Process Communication (IPC): Message passing between processes/threads.
Power Management: ACPI support for shutdown and reboot.
Audio: PC speaker driver for generating tones.
Real-Time Clock (RTC): Time and date functionality.
Performance Monitoring: Kernel profiler for tracking CPU usage and task statistics.
Mutexes: Synchronization primitives for thread safety.

## Requirements
Hardware:
x86-compatible CPU (32-bit).
At least 1 MB of RAM.
VGA-compatible graphics card.
PS/2 mouse and keyboard (optional USB keyboard support).
Floppy disk or CD-ROM drive for booting.

Software:
QEMU emulator (recommended for testing).
i686-elf-gcc cross-compiler.
NASM assembler.
GNU Make and other standard build tools.
genisoimage for creating bootable ISO images.

Building Corea

Clone the Repository:
git clone <repository-url>
cd minios
Install Dependencies: Ensure i686-elf-gcc, nasm, and genisoimage are installed. On Ubuntu, you can install dependencies with:

sudo apt update
sudo apt install build-essential nasm genisoimage

For the cross-compiler, follow instructions to set up i686-elf-gcc (e.g., via OSDev wiki).
Build the OS: Run the provided build script:

chmod +x scripts/build.sh
./scripts/build.sh

This generates os.iso in the project root.

Running Corea
Using QEMU: Run the OS in QEMU with networking support:
qemu-system-i386 -cdrom os.iso -netdev user,id=net0 -device rtl8139,netdev=net0
On Real Hardware: Burn os.iso to a CD or write it to a floppy disk, then boot from the media on an x86 machine.

## Usage

Shell Commands:
clear: Clear the screen.
echo <text>: Print text to the screen.
create <file>: Create a file in VFS (RAMFS or disk).
read <file>: Read file contents.
write <file> <content>: Write to a file.
ls <path>: List files in a VFS path (e.g., /ram or /disk).
ping <data>: Send a network packet.
time: Display current time via RTC.
run <program>: Load and execute a program.
gui: Launch the graphical interface with a sample window and button.
shutdown: Power off the system.
reboot: Restart the system.
tone <freq> <duration>: Play a tone via PC speaker.
script <file>: Execute a script file with multiple commands.
profile: Display CPU usage statistics.



## GUI Interaction:
Use the PS/2 mouse to move the cursor.
Click buttons in windows (e.g., "OK" button in the sample GUI).
Keyboard input is captured for text entry (future apps can extend this).

## Directory Structure

src/boot/: Bootloader code (boot.asm).
src/kernel/: Kernel and driver source files (e.g., kernel.c, gui.c, vfs.c).
src/linker/: Linker script (linker.ld).
scripts/: Build script (build.sh).
README.md: This file.

## Contributing

Contributions are welcome! To contribute:
Fork the repository.
Create a feature branch (git checkout -b feature-name).
Commit changes (git commit -m "Add feature").
Push to the branch (git push origin feature-name).
Open a pull request.


Please ensure code follows the existing style and includes error handling for reliability.

## License

Corea is released under the MIT License. See LICENSE file (to be added) for details.

## Acknowledgments

Built with tools like QEMU, GCC, and NASM.
