/* CoreA Memory Benchmark (C++)
 * Tests shared memory allocation and access
 */
#include <kernel/syscall.h>
#include <stdio.h>
#include <config.h>

int main() {
#ifdef CONFIG_IPC_SHM
    void *addr = NULL;
    size_t size = 1024 * 1024; /* 1MB */
    if (sys_shm(&addr, size) == 0) {
        char *buf = (char *)addr;
        for (size_t i = 0; i < size; ++i) {
            buf[i] = (char)i;
        }
        char msg[64];
        snprintf(msg, sizeof(msg), "Memory Benchmark: Wrote %zu bytes\n", size);
        sys_write(1, msg, strlen(msg));
    } else {
        sys_write(1, "Memory Benchmark: SHM failed\n", 28);
    }
#else
    sys_write(1, "Memory Benchmark: SHM disabled\n", 30);
#endif
    sys_exit(0);
    return 0;
}
