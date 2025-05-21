#include "validator.h"
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

// Simulate bootloader checksum (replace with real bootloader read)
unsigned int calc_checksum(void *boot_sector, unsigned int size) {
    unsigned char *data = (unsigned char *)boot_sector;
    unsigned int sum = 0;
    for (unsigned int i = 0; i < size; i++) {
        sum += data[i];
    }
    return sum;
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

    // Simulate bootloader sector (512 bytes)
    unsigned char boot_sector[512];
    for (int i = 0; i < 512; i++) {
        boot_sector[i] = (unsigned char)(i % 256); // Dummy data
    }

    // Calculate checksum
    unsigned int checksum = calc_checksum(boot_sector, 512);

    // Store checksum in shared memory
    *(unsigned int *)shm_addr = checksum;

    // Report result
    char buf[100];
    snprintf(buf, sizeof(buf), "Boot Checksum: 0x%X\n", checksum);
    sys_write(1, buf, strlen(buf));

    // Cleanup
    sys_exit(0);
    return 0;
}
