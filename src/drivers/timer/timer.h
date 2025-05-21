#ifndef TIMER_H
#define TIMER_H

// System call wrappers (simplified, assumes syscall.h exists)
void *sys_shm(unsigned int size);
void sys_write(int fd, const char *buf, unsigned int count);
void sys_exit(int status);

// External system call for configuration check
int system(const char *command);

#endif
