#ifndef __HAVE_LIQUIDC
#define __HAVE_LIQUIDC

// Basic macros

// Need to break this into two levels to ensure predicate p is macro expanded
#define REF(p)             SREF(p)
#define SREF(p)            __attribute__ ((lcc_predicate (#p)))

#define NNREF(p)           REF((V != 0) => (p))

#define ARRAY              __attribute__ ((array))

#define SHAPE_IGNORE_BOUND __attribute__ ((lcc_ignore_bound))

#define FINAL              __attribute__ ((lcc_final))
#define LOC(l)             __attribute__ ((lcc_sloc (#l)))
#define GLOBAL(l)          __attribute__ ((lcc_global_loc (#l)))
#define OKEXTERN           __attribute__ ((lcc_extern_ok))
#define CHECK_TYPE         __attribute__ ((lcc_check_type))

#define INST(l, k)         __attribute__ ((lcc_inst_sloc (#l, #k)))

#define ROOM_FOR(t)        __attribute__ ((lcc_room_for (sizeof(t))))
#define NNROOM_FOR(t)      __attribute__ ((lcc_nonnull_room_for (sizeof(t))))

// Hack: CIL doesn't allow types as attribute parameters, so we
//       indirect through sizeof.
#define LAYOUT(t)          __attribute__ ((lcc_layout (sizeof(t))))


// Predicate abbreviations

#define PNONNULL       V > 0
#define PVALIDPTR      && [PNONNULL; BLOCK_BEGIN([V]) <= V; V < BLOCK_END([V])]
#define PSTART         V = BLOCK_BEGIN([V])
#define PSIZE(n)       (V + n) = BLOCK_END([V])
#define PSIZE_GE(n)    (V + n) <= BLOCK_END([V])
#define POFFSET(n)     V = (BLOCK_BEGIN([V]) + n)
#define POFFSET_GE(n)  V >= (BLOCK_BEGIN([V]) + n)


// Predicate macros

#define VALIDPTR          REF(PVALIDPTR)
#define NNVALIDPTR        NNREF(PVALIDPTR)
#define NONNULL           REF(PNONNULL)
#define START             REF(PSTART)
#define NNSTART           NNREF(PSTART)
#define SIZE(n)           REF(PSIZE(n))
#define SIZE_GE(n)        REF(PSIZE_GE(n))
#define OFFSET(n)         REF(POFFSET(n))
#define OFFSET_GE(n)      REF(POFFSET_GE(n))
#define NONNEG            REF(V >= 0)

#define STRINGPTR         ARRAY VALIDPTR
#define NNSTRINGPTR       ARRAY NNVALIDPTR
#define ARRAYSTART        ARRAY START VALIDPTR


// Assumptions

#define LCC_VAR2(base, n) base##n
#define LCC_VAR(base, n)  LCC_VAR2(__lcc__##base, n)
#define LCC_ASSUME(p)     ;int LCC_VAR(assume, __COUNTER__ ) = lcc_assume (p);

// Memory Safety Backdoors

#define UNCHECKED              __attribute__ ((lcc_unchecked))

#define LCC_UNSAFE_WRITE(p, d) *((typeof (d) * UNCHECKED) p) = d
#define LCC_UNSAFE_READ(p)     *((typeof (p) UNCHECKED) p)

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

// Built-in functions

extern void validptr (void * VALIDPTR) OKEXTERN;

extern int lcc_assert (int REF(V != 0) p) OKEXTERN;

extern int REF(b = 1) lcc_assume (int b) OKEXTERN;

extern int nondet () OKEXTERN;

extern int REF(V >= 1) nondetpos () OKEXTERN;

extern int REF(V >= 0) nondetnn () OKEXTERN;

// Casts
char * LOC(L) STRINGPTR lcc_check_pos(char * VALIDPTR LOC(L) p) CHECK_TYPE
{
  return p;
}

// Needed for ADPCM

extern int REF(&& [V >= a; V >= b; V >= 0; V <= a + b]) bor (int REF(V >= 0) a, int REF(V >= 0) b) OKEXTERN;

extern int REF(&& [V <= b; V >= 0]) band (int a, int REF(V >= 0) b) OKEXTERN;

#endif
