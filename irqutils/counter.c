#include "counter.h"
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

// Simulate interrupt count (replace with kernel interrupt table query)
unsigned int get_interrupt_count() {
    return 100; // Simulated 100 interrupts
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

    // Get interrupt count
    unsigned int count = get_interrupt_count();

    // Store count in shared memory
    *(unsigned int *)shm_addr = count;

    // Report result
    char buf[100];
    snprintf(buf, sizeof(buf), "Interrupt Count: %u\n", count);
    sys_write(1, buf, strlen(buf));

    // Cleanup
    sys_exit(0);
    return 0;
}
