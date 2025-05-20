/* CoreA System Call Interface
 * Defines system call numbers and prototypes for C programs
 */
#ifndef SYSCALL_H
#define SYSCALL_H

#include <stdint.h>

/* System call numbers */
#define SYS_WRITE 1
#define SYS_EXIT  2
#define SYS_FORK  3
#define SYS_PIPE  4

/* System call prototypes */
int sys_write(int fd, const void *buf, size_t count);
void sys_exit(int status);
int sys_fork(void);
int sys_pipe(int pipefd[2]);

/* Helper macro for system calls (invokes interrupt) */
#define SYSCALL(num, a1, a2, a3) \
    ({ int64_t ret; \
       asm volatile ("int $0x80" \
                     : "=a"(ret) \
                     : "a"(num), "b"(a1), "c"(a2), "d"(a3) \
                     : "memory"); \
       ret; })

#endif /* SYSCALL_H */
