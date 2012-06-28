#ifndef __HAVE_CSOLVE
#define __HAVE_CSOLVE

// Predicate abbreviations
#define PNONNULL       V > 0
#define PNN(p)         ((V != 0) => (p))
#define PVALIDLO       BLOCK_BEGIN([V]) <= V
#define PVALIDHI       V < BLOCK_END([V])

#define PVALIDPTR      && [PNONNULL; PVALIDLO; PVALIDHI]

#define PSTART         V = BLOCK_BEGIN([V])
#define PSIZE(n)       (V + n) = BLOCK_END([V])
#define PSIZE_GE(n)    (V + n) <= BLOCK_END([V])
#define POFFSET(n)     V = (BLOCK_BEGIN([V]) + n)
#define POFFSET_GE(n)  V >= (BLOCK_BEGIN([V]) + n)

#define PEQBLOCK(x)    && [(BLOCK_BEGIN([V]) = BLOCK_BEGIN([x])); (BLOCK_END([V]) = BLOCK_END([x]))]
#define PINTO(x,lo,hi) && [PNONNULL; (PEQBLOCK(x));(POFFSET_GE(lo));(PSIZE_GE((hi + 1)))]
#define PINDEX(x,sz)   && [(BLOCK_BEGIN([x]) <= (x + (sz * V))); ((x + sz + (sz * V)) <= BLOCK_END([x]))]

#ifdef CIL
# define CSOLVE_ATTR(a) __attribute__ ((a))
#else
# define CSOLVE_ATTR(a)
#endif

// Basic macros
// Need to break this into two levels to ensure predicate p is macro expanded
#define SREF(p)            CSOLVE_ATTR (csolve_predicate (#p))
#define REF(p)             SREF(p)
#define NNREF(p)           REF(PNN(p))
#define ARRAY              CSOLVE_ATTR (array)
#define SINGLE             CSOLVE_ATTR (csolve_single)
#define SHAPE_IGNORE_BOUND CSOLVE_ATTR (csolve_ignore_bound)
#define IGNORE_INDEX       CSOLVE_ATTR (csolve_ignore_index)
#define IGNORE_REF_INDEX   IGNORE_INDEX REF(0 = 0) 
#define USE_INDEX          CSOLVE_ATTR (csolve_use_index)
#define ANYREF             CSOLVE_ATTR (csolve_any_ref)
#define ANY                CSOLVE_ATTR (csolve_any_type)
#define NULL               CSOLVE_ATTR (csolve_null_or_ok)
#define VAR(a)             CSOLVE_ATTR (csolve_type_var (#a))

#define FINAL              CSOLVE_ATTR (csolve_final)
#define LOC(l)             CSOLVE_ATTR (csolve_sloc (#l))
#define GLOBAL(l)          CSOLVE_ATTR (csolve_global_loc (#l))
#define OKEXTERN           CSOLVE_ATTR (csolve_extern_ok)
#define CHECK_TYPE         CSOLVE_ATTR (csolve_check_type)
#define INST(l, k)         CSOLVE_ATTR (csolve_inst_sloc (#l, #k))
#define INST_VAR(t, u)     CSOLVE_ATTR (csolve_inst_tvar (#t, #u))
#define ROOM_FOR(t)        CSOLVE_ATTR (csolve_room_for (sizeof(t)))
#define NNROOM_FOR(t)      CSOLVE_ATTR (csolve_nonnull_room_for (sizeof(t)))

#define HASROOM            CSOLVE_ATTR (csolve_has_room)
#define NNHASROOM          CSOLVE_ATTR (csolve_nonnull_has_room)


#define EFFECT(l, p)       CSOLVE_ATTR (csolve_effect (#l, #p))

// Hack: CIL doesn't allow types as attribute parameters, so we
//       indirect through sizeof.
#define LAYOUT(t)          CSOLVE_ATTR (csolve_layout (sizeof(t)))


// Predicate macros

#define VALIDPTR          REF(PVALIDPTR)
#define NNVALIDPTR        NNREF(PVALIDPTR)
#define NONNULL           REF(PNONNULL)
#define START             REF(PSTART)
#define NNSTART           NNREF(PSTART)
#define SIZE(n)           VALIDPTR REF(PSIZE(n))
#define SIZE_GE(n)        VALIDPTR REF(PSIZE_GE(n))
#define NNSIZE(n)         NNREF(PSIZE(n))
#define NNSIZE_GE(n)      NNREF(PSIZE_GE(n))
#define OFFSET(n)         REF(POFFSET(n))
#define OFFSET_GE(n)      REF(POFFSET_GE(n))
#define NONNEG            REF(V >= 0)

#define STRINGPTR         ARRAY VALIDPTR
#define NNSTRINGPTR       ARRAY NNVALIDPTR
#define ARRAYSTART        ARRAY START VALIDPTR
#define OK                START VALIDPTR HASROOM
#define NNOK              NNSTART NNVALIDPTR NNHASROOM          
#define SOMEPTR           REF(0 = 0)

#define NULLTERMSTR       REF((VVADDR = (BLOCK_END([VVADDR]) - 1)) => (V = 0))

typedef char NULLTERMSTR * LOC(L) STRINGPTR csolve_string;
typedef char const NULLTERMSTR FINAL * STRINGPTR  csolve_const_string;

// Assumptions

#define CSOLVE_VAR2(base, n) base##n
#define CSOLVE_VAR(base, n)  CSOLVE_VAR2(__csolve__##base, n)
#define CSOLVE_ASSUME(p)     ;int CSOLVE_VAR(assume, __COUNTER__ ) = csolve_assume (p);

// Memory Safety Backdoors

#define UNCHECKED              CSOLVE_ATTR (csolve_unchecked)

#define CSOLVE_UNSAFE_WRITE(p, d) *((typeof (d) * UNCHECKED) p) = d
#define CSOLVE_UNSAFE_READ(p)     *((typeof (p) UNCHECKED) p)

// Built-in functions

extern void csolve_fold_all () OKEXTERN;


extern void validptr (void * VALIDPTR IGNORE_INDEX) OKEXTERN;
extern void csolve_validptr_lo (void * REF(PVALIDLO) IGNORE_INDEX) OKEXTERN;
extern void csolve_validptr_hi (void * REF(PVALIDHI) IGNORE_INDEX) OKEXTERN;

extern int csolve_assert (int REF(V != 0) p) OKEXTERN;

extern void * LOC(L) REF(V = BLOCK_BEGIN([p])) csolve_block_begin (void * LOC(L) IGNORE_REF_INDEX p) OKEXTERN;

extern void * LOC(L) REF(V = BLOCK_END([p])) csolve_block_end (void * LOC(L) IGNORE_REF_INDEX p) OKEXTERN;

extern int REF(b = 1) csolve_assume (int b) OKEXTERN;

extern int nondet () OKEXTERN;

extern int REF(V >= 1) USE_INDEX nondetpos () OKEXTERN;

extern int REF(V >= 0) USE_INDEX nondetnn () OKEXTERN;

extern int REF(V >= l) REF(V < u) REF(V >= 0) USE_INDEX nondetrange (int l, int REF(l < V) u) OKEXTERN;

// Casts
//char * LOC(L) STRINGPTR csolve_check_pos(char * LOC(L) VALIDPTR p) CHECK_TYPE
//{
//  return p;
//}

// Needed for ADPCM -- so put these over there ?
extern int REF(&& [V >= a; V >= b; V >= 0; V <= a + b]) bor (int REF(V >= 0) a, int REF(V >= 0) b) OKEXTERN;

extern int REF(&& [V <= b; V >= 0]) band (int a, int REF(V >= 0) b) OKEXTERN;

extern int REF(&& [V < m; V >= 0; V <= a]) csolve_mod (int REF(V >= 0) IGNORE_INDEX a, int REF(V > 0) IGNORE_INDEX m) OKEXTERN;

extern int REF(&& [ (V = (x * y))
                  ; (V = (y * x))
                  ; (((x * y) + y) = ((x + 1) * y))
                  ; (((x * y) + x) = (x * (y + 1)))
                  ; ((&&[(x > 0); (y > 0)]) => (&&[(V >= x); (V >= y)]))
                  ; ((x = 0) => (V = 0))
                  ; ((y = 0) => (V = 0))
                  ]) USE_INDEX 
  csolve_times (int x, int y) OKEXTERN;

extern int REF((a * c) <= (b * c)) USE_INDEX 
  csolve_axiom_times_mono (int REF(0 <= V) a, int REF(a <= V) b, int REF(0 <= V) c) OKEXTERN;

#endif
