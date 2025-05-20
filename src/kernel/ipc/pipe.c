/* CoreA Pipe IPC Implementation
 * Implements the sys_pipe system call
 */
#include <kernel/ipc/pipe.h>
#include <kernel/vfs.h>
#include <kernel/heap.h>
#include <kernel/config.h>

#ifdef CONFIG_IPC_PIPE

static struct pipe *pipe_create(void) {
    struct pipe *pipe = kmalloc(sizeof(struct pipe));
    if (!pipe) return NULL;
    pipe->read_pos = 0;
    pipe->write_pos = 0;
    pipe->count = 0;
    pipe->ref_count = 2; /* One for each file descriptor */
    return pipe;
}

int sys_pipe(int pipefd[2]) {
    struct pipe *pipe = pipe_create();
    if (!pipe) return -1;

    /* Create two file descriptors: read and write */
    pipefd[0] = vfs_open_pipe(pipe, 0); /* Read end */
    pipefd[1] = vfs_open_pipe(pipe, 1); /* Write end */
    if (pipefd[0] < 0 || pipefd[1] < 0) {
        kfree(pipe);
        return -1;
    }

    return 0;
}

#else
int sys_pipe(int pipefd[2]) {
    return -1; /* Pipe not supported */
}
#endif
