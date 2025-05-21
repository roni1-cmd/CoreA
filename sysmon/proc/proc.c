#include "proc.h"
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

// Simulate process count (replace with real kernel process table query)
unsigned int get_process_count() {
    return 5; // Simulated 5 processes
}

int main() {
    // Check kernel configuration
    char conf_cmd[] = "perl -e 'exit 1 unless do \"config/kernel.conf\"->{SHM} && do \"config/kernel.conf\"->{PROCESS}'";
    if (system(conf_cmd) != 0) {
        char *err = "Required features disabled\n";
        sys_write(1, err, strlen(err));
        sys_exit(1);
    }

    // Allocate shared memory
    void *shm_addr = sys_shm(1024);
    if (!shm_addr) {
        char *err = "SHM allocation failed\n";
        sys_write(1, err, strlen(err));
        sys_exit(1);
    }

    // Fork to simulate process activity
    for (int i = 0; i < 3; i++) {
        int pid = sys_fork();
        if (pid == -1) {
            char *err = "Fork failed\n";
            sys_write(1, err, strlen(err));
            sys_exit(1);
        }
        if (pid == 0) {
            // Child process: exit immediately
            sys_exit(0);
        }
    }

    // Get process count
    unsigned int count = get_process_count();

    // Store count in shared memory
    *(unsigned int *)shm_addr = count;

    // Report result
    char buf[100];
    snprintf(buf, sizeof(buf), "Active Processes: %u\n", count);
    sys_write(1, buf, strlen(buf));

    // Cleanup
    sys_exit(0);
    return 0;
}
