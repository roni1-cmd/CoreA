#ifndef PROC_H
#define PROC_H

// System call wrappers
void *sys_shm(unsigned int size);
void sys_write(int fd, const char *buf, unsigned int count);
void sys_exit(int status);
int sys_fork();

// External system call for configuration check
int system(const char *command);

#endif
