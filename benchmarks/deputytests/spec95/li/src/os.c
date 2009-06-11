/* unixstuff.c - unix specific routines */

#include "xlisp.h"
#include "proto.h"

/* osinit - initialize */
void osinit(char *banner)
{
    printf("%s\n",banner);
}

/* osrand - return a random number between 0 and n-1 */
int osrand(int n)
{
    return((int)(rand()/4294967296.0 * (double)n));
}

/* osgetc - get a character from the terminal */
int osgetc(FILE *fp)
{
    return(getc(fp));
}

/* osputc - put a character to the terminal */
void osputc(int ch,FILE *fp)
{
    putc(ch, fp);
}

/* oscheck - check for control characters during execution */
void oscheck(void)
{
    /* NIX */
}

/* osfinit - initialize pc specific functions */
void osfinit(void)
{
    /* NIX */
}

/* osfinish - cleanup before exit */
void osfinish(void)
{
    /* NIX */
}

