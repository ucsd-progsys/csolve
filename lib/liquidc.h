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
#define NONNULL           REF(PNONNULL)
#define START             REF(PSTART)
#define STARTifNONNULL    REF((V != 0) => PSTART)
#define SIZE(n)           REF(PSIZE(n))
#define SIZE_GE(n)        REF(PSIZE_GE(n))
#define OFFSET(n)         REF(POFFSET(n))
#define OFFSET_GE(n)      REF(POFFSET_GE(n))
#define NONNEG            REF(V >= 0)
#define PTR_TO_ONE(t)     t * VALIDPTR START ROOM_FOR(t)


// Built-in functions

extern void validptr (void * VALIDPTR) OKEXTERN;

extern int lcc_assert (int REF(V != 0) p) OKEXTERN;

extern int REF(b = 1) lcc_assume (int b) OKEXTERN;

extern int nondet () OKEXTERN;

extern int REF(V > 0) nondetpos () OKEXTERN;

extern int REF(V >= 0) nondetnn () OKEXTERN;

// Needed for ADPCM

extern int REF(&& [V >= a; V >= b; V >= 0; V <= a + b]) bor (int REF(V >= 0) a, int REF(V >= 0) b) OKEXTERN;

extern int REF(&& [V <= b; V >= 0]) band (int a, int REF(V >= 0) b) OKEXTERN;

#endif
