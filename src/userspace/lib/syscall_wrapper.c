/* CoreA System Call Wrapper
 * Provides C functions for COBOL and Haskell to invoke system calls
 */
#include <kernel/syscall.h>
#include <stdint.h>

int sys_write_c(int fd, const char *buf, size_t count) {
    return sys_write(fd, buf, count);
}

void sys_exit_c(int status) {
    sys_exit(status);
}

int sys_fork_c(void) {
    return sys_fork();
}

int sys_pipe_c(int *pipefd) {
    return sys_pipe(pipefd);
}

int sys_semaphore_c(int *sem_id, int value) {
    return sys_semaphore(sem_id, value);
}

int sys_mutex_c(int *mutex_id, int lock) {
    return sys_mutex(mutex_id, lock);
}

int sys_shm_c(void **addr, size_t size) {
    return sys_shm(addr, size);
}
