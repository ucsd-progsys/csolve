/* Various useful definitions */

extern void *malloc(int);

extern void assert(int);
extern void exit(int);

extern long int atol(char *);

extern char *strtok(char *, char *);

#define NULL   0

#define FILE   int

#define stdout 0
#define stdin  1
#define stderr 2

extern FILE *fopen(char *, char *);
extern int fprintf(FILE *, char *, ...);
extern char *fgets(char *, int, FILE *);

extern int sscanf(char *, char *, ...);
