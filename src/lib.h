/* Various useful definitions */

extern void *malloc(int);

extern void assert(int);
extern void exit(int);

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

extern int sscanf(char * __attribute__ ((array)), char * __attribute__ ((array)), ...);

#define CLOCKS_PER_SEC 1000

typedef long int clock_t;

extern clock_t clock(void);

extern double sqrt(double);
extern double fabs(double);
