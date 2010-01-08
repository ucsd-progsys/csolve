/* Various useful definitions */

typedef unsigned int size_t;

extern void *malloc(size_t);

extern int bor(int, int);
extern int band(int, int);

extern void validptr(void *);

extern long random(void) ;
extern void assert(int);
extern int assume(int);
extern void exit(int);
extern int csolve_exit(int);

#define INT_MIN -2147483648
#define INT_MAX 2147483647

#define LONG_MIN -2147483648
#define LONG_MAX 2147483647

extern int errno;

#define EPERM            1      /* Operation not permitted */
#define ENOENT           2      /* No such file or directory */
#define ESRCH            3      /* No such process */
#define EINTR            4      /* Interrupted system call */
#define EIO              5      /* I/O error */
#define ENXIO            6      /* No such device or address */
#define E2BIG            7      /* Argument list too long */
#define ENOEXEC          8      /* Exec format error */
#define EBADF            9      /* Bad file number */
#define ECHILD          10      /* No child processes */
#define EAGAIN          11      /* Try again */
#define ENOMEM          12      /* Out of memory */
#define EACCES          13      /* Permission denied */
#define EFAULT          14      /* Bad address */
#define ENOTBLK         15      /* Block device required */
#define EBUSY           16      /* Device or resource busy */
#define EEXIST          17      /* File exists */
#define EXDEV           18      /* Cross-device link */
#define ENODEV          19      /* No such device */
#define ENOTDIR         20      /* Not a directory */
#define EISDIR          21      /* Is a directory */
#define EINVAL          22      /* Invalid argument */
#define ENFILE          23      /* File table overflow */
#define EMFILE          24      /* Too many open files */
#define ENOTTY          25      /* Not a typewriter */
#define ETXTBSY         26      /* Text file busy */
#define EFBIG           27      /* File too large */
#define ENOSPC          28      /* No space left on device */
#define ESPIPE          29      /* Illegal seek */
#define EROFS           30      /* Read-only file system */
#define EMLINK          31      /* Too many links */
#define EPIPE           32      /* Broken pipe */
#define EDOM            33      /* Math argument out of domain of func */
#define ERANGE          34      /* Math result not representable */

extern long int atol(char * __attribute__ ((array)));

extern char *strtok(char * __attribute__ ((array)), char * __attribute__ ((array)));

#define NULL   0

#define FILE   int

#define stdout (FILE *)0
#define stdin  (FILE *)1
#define stderr (FILE *)2

extern FILE *fopen(char * __attribute__ ((array)), char * __attribute__ ((array)));
extern int fprintf(FILE *, char * __attribute__ ((array)), ...);
extern int printf(char * __attribute__ ((array)), ...);
extern char *fgets(char * __attribute__ ((array)), int, FILE *);
extern int *getc(FILE *);
extern int sscanf(char * __attribute__ ((array)), char * __attribute__ ((array)), ...);

#define CLOCKS_PER_SEC 1000

typedef long int clock_t;

extern clock_t clock(void);

extern double sqrt(double);
extern double fabs(double);
