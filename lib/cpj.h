#include <liquidc.h>

#ifdef  __HAVE_LIQUIDC
#ifndef __HAVE_CPJ
#define __HAVE_CPJ

#ifdef  __MAKE_SEQ
  #define BLOCKATTRIBUTE(x)  
#else
  #define BLOCKATTRIBUTE(x) __blockattribute__ (x)
#endif


// Deterministic Parallel Constructs

#define COBEGIN                while (1) { BLOCKATTRIBUTE((lcc_cobegin))
#define RTBEG                  if (nondet ()) { BLOCKATTRIBUTE((lcc_coroutine))
#define RTEND                  }
#define RTN(s)                 RTBEG s; RTEND
#define COEND                  RTN(break)}

#define FOREACH(i, l, u)       while (nondet ()) { BLOCKATTRIBUTE((lcc_foreach)) \
                                 int i = nondetrange(l, u); \
                                 lcc_assert(l <= u); \
                                 { BLOCKATTRIBUTE((lcc_foreach_iter))
#define ENDFOR                 }}

// more natural looking macros for parallel constructs

#define foreach(i, l, u)       FOREACH(i, l, u)
#define endfor                 ENDFOR
#define cobegin                COBEGIN
#define coend                  COEND
#define rtbeg                  RTBEG
#define rtend                  RTEND
#define rtn(s)                 RTN(s)


// porting is even more of a pain without booleans
typedef int bool;

#define FALSE 0
#define TRUE  1

#define ISTRUE(x) (x != 0)
#define ISFALSE(x) (x == 0)

// Locally Unique Arrays

#define UNQ

#endif
#endif
