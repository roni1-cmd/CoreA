#include "timer.h"
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

// Simulate hardware timer register (replace with real hardware access if available)
volatile unsigned int timer_ticks = 0;

// Main function
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

    // Simulate timer ticks
    for (int i = 0; i < 10; i++) {
        timer_ticks += 100; // Increment ticks (simulate hardware)
        *(unsigned int *)shm_addr = timer_ticks; // Store in shared memory

        // Prepare output
        char buf[100];
        snprintf(buf, sizeof(buf), "Timer Ticks: %u\n", timer_ticks);
        sys_write(1, buf, strlen(buf));
    }

    // Cleanup
    sys_exit(0);
    return 0;
}
