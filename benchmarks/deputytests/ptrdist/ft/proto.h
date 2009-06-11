// proto.h
// sm: collecting together prototypes for functions which are
// ordinarily declared in stdlib.h; stdlib.h itself is a problem
// because of its conflicting definitions of the random functions

#ifndef PROTO_H
#define PROTO_H

#ifndef DEPUTY
  #define TRUSTED
  #define COUNT(n)
  #define NT
  #define NTS
  #define DALLOC(n)
  #define BND(x,y)
#endif

int atoi(char const * NTS str);

#ifdef __CYGWIN__
  #define srandom_return_type long
  #define random_return_type  int
#else
  #define srandom_return_type void
  #define random_return_type  long
#endif
srandom_return_type srandom(unsigned int x);
random_return_type random();

// gn: I wish I could include stdlib. But it has conflicts with
// random.c
// #include <stdlib.h> // malloc
extern void* (DALLOC(size) malloc)(unsigned int size);

extern void  free(void * TRUSTED);

void exit(int exitValue);


#endif // PROTO_H
