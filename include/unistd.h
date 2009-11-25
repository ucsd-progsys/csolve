#ifndef __UNISTD_H__
#define __UNISTD_H__

#include <stdlib.h>

extern ssize_t read(int fildes, void *buf, size_t nbyte);
extern ssize_t write(int fildes, const void *buf, size_t nbyte);

#endif
