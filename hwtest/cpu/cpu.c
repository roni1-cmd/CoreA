#include "cpu.h"
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

// Simulate CPUID (replace with real CPUID instruction for actual hardware)
void get_cpuid(unsigned int eax_in, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx) {
    // Simulated CPUID for demonstration
    *eax = 0x1; // Version info
    *ebx = 0x756E6547; // "Genu" (Intel signature)
    *ecx = 0x1; // SSE support
    *edx = 0x1; // FPU support
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

    // Get CPUID info
    unsigned int eax, ebx, ecx, edx;
    get_cpuid(1, &eax, &ebx, &ecx, &edx);

    // Store features in shared memory
    unsigned int *shm = (unsigned int *)shm_addr;
    shm[0] = eax; // Version
    shm[1] = ecx; // Features (e.g., SSE)
    shm[2] = edx; // Features (e.g., FPU)

    // Report features
    char buf[100];
    snprintf(buf, sizeof(buf), "CPU Features: SSE=%d, FPU=%d\n", (ecx & 0x1), (edx & 0x1));
    sys_write(1, buf, strlen(buf));

    // Cleanup
    sys_exit(0);
    return 0;
}
