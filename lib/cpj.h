#include <csolve.h>

#ifndef __HAVE_CPJ
#define __HAVE_CPJ

#ifdef  __MAKE_SEQ
  #define BLOCKATTRIBUTE(x)  
#else
  #define BLOCKATTRIBUTE(x) __blockattribute__ (x)
#endif

// Deterministic Parallel Constructs

#define COBEGIN                while (1) { BLOCKATTRIBUTE((csolve_cobegin))
#define RTBEG                  if (nondet ()) { BLOCKATTRIBUTE((csolve_coroutine)) csolve_fold_all ();
#define RTEND                  }
#define RTN(s)                 RTBEG s; RTEND
#define COEND                  if (nondet ()) { break; } }

#define FOREACH(i, l, u)       FOREACH2(i, l, u, __COUNTER__)
#define FOREACH2(i, l, u, x)   FOREACH3(i, l, u, x)
#define FOREACH3(i, l, u, x)   {  int i;                       \
                                  int __csolve_foreach_lb_##x = l; \
                                  int __csolve_foreach_ub_##x = u; \
                                  while (nondet ()) { BLOCKATTRIBUTE((csolve_foreach)) \
                                    i = nondetrange(__csolve_foreach_lb_##x, __csolve_foreach_ub_##x); \
                                    { BLOCKATTRIBUTE((csolve_foreach_iter)) csolve_fold_all ();
#define ENDFOR                 }}}

#define ATOMIC                 BLOCKATTRIBUTE((csolve_atomic))

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

// Effect Declarations

#define PRAGMA                      #pragma
#define CSOLVE_EFFECT(e)               PRAGMA csolve_effect_decl (#e)
#define CSOLVE_EFFECTS_COMMUTE(e1, e2) PRAGMA csolve_effects_commute (#e1, #e2)

// Allocators

extern
float * NONNULL SIZE(4 * sz2) START ARRAY
      * NONNULL SIZE(4 * sz1) START ARRAY
  mallocFloatMatrix (int REF(V > 0) sz1, int REF(V > 0) sz2)
  OKEXTERN;

extern
int * NONNULL SIZE(4 * sz2) START ARRAY
    * NONNULL SIZE(4 * sz1) START ARRAY
  mallocIntMatrix (int REF(V > 0) sz1, int REF(V > 0) sz2)
  OKEXTERN;

extern
int REF(V = 0) * NONNULL SIZE(4 * sz) START ARRAY
  callocInt(int REF(V > 0) sz)
  OKEXTERN;
#endif
