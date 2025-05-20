# CoreA System Call API

This document describes the **system call interface** for the **CoreA Operating System**, providing access to kernel services such as **file I/O**, **process management**, and **inter-process communication (IPC)**.

System calls are available from **C**, **Perl**, and **Assembly**, allowing flexible usage across languages.

---

## 🛠️ System Calls

---

### `sys_write` – Write Data to a File Descriptor

#### C
```c
int sys_write(int fd, const void *buf, size_t count);
```
- `fd`: File descriptor (e.g., `1` for stdout)  
- `buf`: Pointer to data buffer  
- `count`: Number of bytes to write  
- **Returns**: Number of bytes written, or `-1` on error

#### Perl
```perl
use CoreA::Syscall;
my $bytes_written = sys_write($fd, $buffer, $count);
```
- `$fd`: File descriptor  
- `$buffer`: String or buffer to write  
- `$count`: Length of data  
- **Returns**: Number of bytes written, or dies on error

#### Assembly
```asm
SYSCALL3 SYS_WRITE, rdi, rsi, rdx
```
- `rdi`: File descriptor  
- `rsi`: Buffer address  
- `rdx`: Byte count  
- **Returns**: Bytes written in `rax`

---

### `sys_exit` – Terminate the Calling Process

#### C
```c
void sys_exit(int status);
```
- `status`: Exit status code  
- **Does not return**

#### Perl
```perl
use CoreA::Syscall;
sys_exit($status);
```
- `$status`: Exit status code  
- **Does not return**

#### Assembly
```asm
SYSCALL1 SYS_EXIT, rdi
```
- `rdi`: Exit status  
- **Does not return**

---

### `sys_fork` – Create a New Process

#### C
```c
int sys_fork(void);
```
- **Returns**: `0` in child, child PID in parent, or `-1` on error  
- **Requires**: `CONFIG_PROCESS` and `CONFIG_SCHEDULER` in `kernel.conf`

#### Perl
```perl
use CoreA::Syscall;
my $pid = sys_fork();
```
- **Returns**: `0` in child, child PID in parent, or dies if not supported

#### Assembly
```asm
SYSCALL0 SYS_FORK
```
- **Returns**: `0` in child, child PID in parent (in `rax`)

---

### `sys_pipe` – Create a Pipe for IPC

#### C
```c
int sys_pipe(int pipefd[2]);
```
- `pipefd`: Array to store read and write file descriptors  
- **Returns**: `0` on success, `-1` on error  
- **Requires**: `CONFIG_IPC_PIPE` in `kernel.conf`

#### Perl
```perl
use CoreA::Syscall;
my @pipefd;
sys_pipe(\@pipefd);
```
- `@pipefd`: Array for read/write descriptors  
- **Returns**: `0` on success, dies on error or if not supported

#### Assembly
```asm
SYSCALL1 SYS_PIPE, rdi
```
- `rdi`: Address of `pipefd` array  
- **Returns**: `0` in `rax` on success

---

## 📘 Usage Notes

- **C**:  
  Include `<kernel/syscall.h>` and use the provided prototypes.  
  Use the `SYSCALL` macro which triggers interrupt `0x80`.

- **Perl**:  
  Use the `CoreA::Syscall` module located at `src/userspace/lib/CoreA/Syscall.pm`.  
  The module checks `config/kernel.conf` for feature availability.

- **Assembly**:  
  Include `syscall.asm`.  
  Use the appropriate macro: `SYSCALL0`, `SYSCALL1`, `SYSCALL2`, or `SYSCALL3` based on argument count.

- **Kernel Configuration**:  
  Some system calls (e.g., `sys_fork`, `sys_pipe`) require kernel features to be enabled.  
  Check `config/kernel.conf` before use.

---

## 🧪 Example Usage

### C
```c
#include <kernel/syscall.h>
#include <stdint.h>

int main() {
    const char *msg = "Hello, CoreA!\n";
    sys_write(1, msg, 14);
    sys_exit(0);
    return 0;
}
```

---

### Perl
```perl
use CoreA::Syscall;

sys_write(1, "Hello, CoreA!\n", 14);
sys_exit(0);
```

---

### Assembly (x86-64 NASM)
```asm
%include "syscall.asm"

section .text
global _start

_start:
    mov rdi, 1              ; stdout
    mov rsi, msg            ; message address
    mov rdx, 14             ; length
    SYSCALL3 SYS_WRITE, rdi, rsi, rdx

    mov rdi, 0              ; exit status
    SYSCALL1 SYS_EXIT, rdi

section .data
msg: db "Hello, CoreA!", 10
```
