#include <liquidc.h>

#ifdef  __HAVE_LIQUIDC
#ifndef __HAVE_CPJ
#define __HAVE_CPJ

// Deterministic Parallel Constructs

#define COBEGIN                { __blockattribute__ ((lcc_cobegin))\
                                 int sw = (int __attribute__ ((lcc_cobegin_index))) nondet();\
                                 switch(sw) {
#define COEND                  }}
#define RTBEG                  RTBEG2( __COUNTER__ )
#define RTBEG2(x)              RTBEG3( x )
#define RTBEG3(x)              case x:\
                                 { __blockattribute__ ((lcc_coroutine_##x)) 
#define RTEND                    } break;
#define RTN(s)                 RTBEG s; RTEND 

#define FOREACH(i, l, u)       FOREACH2(i, l, u, __COUNTER__)
#define FOREACH2(i, l, u, x)   FOREACH3(i, l, u, x)
#define FOREACH3(i, l, u, x)   { __blockattribute__ ((lcc_foreach_##x)) \
                                 ITER(i, l, u, x) {
#define ITER(i, l, u, x)           int i = (int __attribute__ ((lcc_foreach_index_##x))) nondet();\
                                lcc_assume(i >= l && i < u);
#define ENDFOR                 }}

// more natural looking macros for parallel constructs

#define foreach(i, l, u)       FOREACH(i, l, u)
#define endfor                 ENDFOR
#define cobegin                COBEGIN
#define coend                  COEND
#define rtbeg                  RTBEG
#define rtend                  RTEND
#define rtn(s)                 RTN(s)

typedef int bool;

#endif
#endif
