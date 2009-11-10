#ifndef __STDLIB_H__
#define __STDLIB_H__

typedef unsigned int size_t;

typedef size_t ssize_t;

#define NULL   0

extern void *malloc(size_t);

extern long int atol(char * __attribute__ ((array)));

extern void exit(int);

#endif
