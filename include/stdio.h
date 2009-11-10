#ifndef __STDIO_H__
#define __STDIO_H__

#define FILE   int

#define stdout (FILE *)0
#define stdin  (FILE *)1
#define stderr (FILE *)2

extern FILE *fopen(char * __attribute__ ((array)), char * __attribute__ ((array)));
extern int fprintf(FILE *, char * __attribute__ ((array)), ...);
extern int printf(char * __attribute__ ((array)), ...);
extern char *fgets(char * __attribute__ ((array)), int, FILE *);

extern int sscanf(char * __attribute__ ((array)), char * __attribute__ ((array)), ...);

#endif
