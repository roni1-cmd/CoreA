/* CoreA CPU Benchmark (C++)
 * Measures CPU performance with arithmetic loop
 */
#include <kernel/syscall.h>
#include <stdio.h>

int main() {
    const int iterations = 1000000;
    long long result = 0;
    for (int i = 1; i <= iterations; ++i) {
        result += i * 2;
    }
    char buf[64];
    snprintf(buf, sizeof(buf), "CPU Benchmark Result: %lld\n", result);
    sys_write(1, buf, strlen(buf));
    sys_exit(0);
    return 0;
}
