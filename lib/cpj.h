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
#define COEND                  if (nondet ()) { break; } }

#define FOREACH(i, l, u)       FOREACH2(i, l, u, __COUNTER__)
#define FOREACH2(i, l, u, x)   FOREACH3(i, l, u, x)
#define FOREACH3(i, l, u, x)   {  int i;                       \
                                  int __lcc_foreach_lb_##x = l; \
                                  int __lcc_foreach_ub_##x = u; \
                                  while (nondet ()) { BLOCKATTRIBUTE((lcc_foreach)) \
                                    i = nondetrange(__lcc_foreach_lb_##x, __lcc_foreach_ub_##x); \
                                    { BLOCKATTRIBUTE((lcc_foreach_iter))
#define ENDFOR                 }}}

#define ATOMIC                 BLOCKATTRIBUTE((lcc_atomic))

// more natural looking macros for parallel constructs

#define foreach(i, l, u)       FOREACH(i, l, u)
#define endfor                 ENDFOR
#define cobegin                COBEGIN
#define coend                  COEND
#define rtbeg                  RTBEG
#define rtend                  RTEND
#define rtn(s)                 RTN(s)
#define atomic                 ATOMIC


// porting is even more of a pain without booleans
typedef int bool;

#define FALSE 0
#define TRUE  1

#define ISTRUE(x) (x != 0)
#define ISFALSE(x) (x == 0)

// Locally Unique Arrays

#define UNQ

// Effect Declarations

#define PRAGMA                      #pragma
#define LCC_EFFECT(e)               PRAGMA lcc_effect_decl (#e)
#define LCC_EFFECTS_COMMUTE(e1, e2) PRAGMA lcc_effects_commute (#e1, #e2)

#endif
#endif
