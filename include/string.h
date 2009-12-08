#ifndef __STRING_H__
#define __STRING_H__

#include <liquidc.h>

void *memccpy(void *, const void *, int, size_t);
void *memchr(const void *, int, size_t);
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);
char * ARRAY strcat(char * ARRAY , const char * ARRAY );
char * ARRAY strchr(const char * ARRAY , int);
int strcmp(const char * ARRAY , const char * ARRAY );
int strcoll(const char * ARRAY , const char * ARRAY );
char * ARRAY strcpy(char * ARRAY , const char * ARRAY );
size_t strcspn(const char * ARRAY , const char * ARRAY );
char * ARRAY strdup(const char * ARRAY );
char * ARRAY strerror(int);
size_t strlen(const char * ARRAY );
char * ARRAY strncat(char * ARRAY , const char * ARRAY , size_t);
int strncmp(const char * ARRAY , const char * ARRAY , size_t);
char * ARRAY strncpy(char * ARRAY , const char * ARRAY , size_t);
char * ARRAY strpbrk(const char * ARRAY , const char * ARRAY );
char * ARRAY strrchr(const char * ARRAY , int);
size_t strspn(const char * ARRAY , const char * ARRAY );
char * ARRAY strstr(const char * ARRAY , const char * ARRAY );
char * ARRAY strtok(char * ARRAY , const char * ARRAY );
char * ARRAY strtok_r(char * ARRAY , const char * ARRAY , char * ARRAY *);
size_t strxfrm(char * ARRAY , const char * ARRAY , size_t);

char * ARRAY rindex (const char * ARRAY , int);

#endif
