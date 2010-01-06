#ifndef __STDIO_H__
#define __STDIO_H__

#include <stdlib.h>

#define FILE   int

#define stdout (FILE *)0
#define stdin  (FILE *)0
#define stderr (FILE *)0
#define EOF	       0

extern int sscanf(char * __attribute__ ((array)), char * __attribute__ ((array)), ...);
extern FILE *fopen(char * __attribute__ ((array)), char * __attribute__ ((array)));
extern int fprintf(FILE *, char * __attribute__ ((array)), ...);
extern int printf(char * __attribute__ ((array)), ...);
extern char *fgets(char * __attribute__ ((array)), int, FILE *);
extern int putc(int c, FILE *stream);


extern void perror ( const char * str );

extern void     clearerr(FILE *);
extern char    *ctermid(char * __attribute__((array)));
extern char    *cuserid(char * __attribute__((array)));//(LEGACY)
extern int      fclose(FILE *);
extern FILE    *fdopen(int, const char * __attribute__((array)));
extern int      feof(FILE *);
extern int      ferror(FILE *);
extern int      fflush(FILE *);
extern int      fgetc(FILE *);
extern int      fileno(FILE *);
extern void     flockfile(FILE *);
extern int      fputc(int, FILE *);
extern int      fputs(const char * __attribute__((array)), FILE *);
extern size_t   fread(void *, size_t, size_t, FILE *);
extern FILE    *freopen(const char * __attribute__((array)), const char * __attribute__((array)), FILE *);
extern int      fscanf(FILE *, const char * __attribute__((array)), ...);
extern int      fseek(FILE *, long int, int);
extern long int ftell(FILE *);
extern int      ftrylockfile(FILE *);
extern void     funlockfile(FILE *);
extern size_t   fwrite(const void *, size_t, size_t, FILE *);
extern int      getc(FILE *);
extern int      getchar(void);
extern int      getc_unlocked(FILE *);
extern int      getchar_unlocked(void);
extern char    *gets(char * __attribute__((array)));
extern int      getw(FILE *);
extern int      pclose(FILE *);
extern FILE    *popen(const char * __attribute__((array)), const char * __attribute__((array)));
extern int      putchar(int);
extern int      putc_unlocked(int, FILE *);
extern int      putchar_unlocked(int);
extern int      puts(const char * __attribute__((array)));
extern int      putw(int, FILE *);
extern int      remove(const char * __attribute__((array)));
extern int      rename(const char * __attribute__((array)), const char * __attribute__((array)));
extern void     rewind(FILE *);
extern int      scanf(const char * __attribute__((array)), ...);
extern void     setbuf(FILE *, char * __attribute__((array)));
extern int      setvbuf(FILE *, char * __attribute__((array)), int, size_t);
extern int      snprintf(char * __attribute__((array)), size_t, const char * __attribute__((array)), ...);
extern int      sprintf(char * __attribute__((array)), const char * __attribute__((array)), ...);
extern char    *tempnam(const char * __attribute__((array)), const char * __attribute__((array)));
extern FILE    *tmpfile(void);
extern char    *tmpnam(char * __attribute__((array)));
extern int      ungetc(int, FILE *);

//extern int      fgetpos(FILE *, fpos_t *);
//extern char    *fgets(char * __attribute__((array)), int, FILE *);
//extern FILE    *fopen(const char * __attribute__((array)), const char * __attribute__((array)));
//extern int      fprintf(FILE *, const char * __attribute__((array)), ...);
//extern void     perror(const char * __attribute__((array)));
//extern int      printf(const char * __attribute__((array)), ...);
//extern int      putc(int, FILE *);
//extern int      fseeko(FILE *, off_t, int);
//extern int      fsetpos(FILE *, const fpos_t *);
//extern off_t    ftello(FILE *);
//extern int      sscanf(const char * __attribute__((array)), const char * __attribute__((array)), int , ...);
//extern int      vfprintf(FILE *, const char * __attribute__((array)), va_list);
//extern int      vprintf(const char * __attribute__((array)), va_list);
//extern int      vsnprintf(char * __attribute__((array)), size_t, const char * __attribute__((array)), va_list);
//extern int      vsprintf(char * __attribute__((array)), const char * __attribute__((array)), va_list);

#endif
