//! run-with --nop
// Scratchpad for testing types-as-specs

// Basic macros

// Need to break this into two levels to ensure predicate p is macro expanded
#define REF(p)     SREF(p)
#define SREF(p)    __attribute__ ((lcc_predicate (#p)))

#define LOC(l)     __attribute__ ((lcc_sloc (#l)))
#define GLOBAL(l)  __attribute__ ((lcc_global_loc (#l)))
#define CONCRETE   __attribute__ ((lcc_concrete))
#define OKEXTERN   __attribute__ ((lcc_extern_ok))
#define CHECK_TYPE __attribute__ ((lcc_check_type))

// Predicate abbreviations

#define PNONNULL  V > 0
#define PVALIDPTR && [PNONNULL; BLOCK_BEGIN([V]) <= V; V < BLOCK_END([V])]
#define PSTART    V = BLOCK_BEGIN([V])
#define PSIZE(n)  (V + n) <= BLOCK_END([V])

// Predicate macros

#define VALIDPTR REF(PVALIDPTR)
#define NONNULL  REF(PNONNULL)
#define START    REF(PSTART)
#define SIZE(n)  REF(PSIZE(n))

extern void pmr_assert (int REF(V != 0)) OKEXTERN;

extern void assert (int REF(V != 0)) OKEXTERN;

int *globalPointer;

int globalArray[10];

// Used to have y >= 0, but causes problems for index checking
int REF(V > x) test (int x, int REF(&& [V > x]) y) CHECK_TYPE {
    pmr_assert (y > x);
    /* pmr_assert (y >= 0); */

    return y;
}

void testtest () {
    int z = test (0, 1);

    assert (z > 0);
}

typedef struct {
    int fst;
    int REF(V > fst) snd;
} pair;

pair globalPairArray[5];

void testptr (int REF(V > 0) *x) {
    assert (*x > 0);
}

void testptr2 (int * LOC(L) x, int * LOC(L) y) {
}

void testptr3 (int *x, int *y) {
}

int *testptr4 () {
    return 0;
}

void testpair (pair *p) {
    assert (p->snd > p->fst);
}

typedef struct {
    int x;
    char c;
    int foo[3];
} arrayStruct1;

typedef struct {
    int x;
    char c;
    int foo[];
} arrayStruct2;

void testArrayStruct (arrayStruct1 *a1, arrayStruct2 *a2) {
}

typedef struct {
    int y;
    arrayStruct1 as1;
    int z;
} nestedStruct;

void testNestedStruct (nestedStruct *ns) {
}

void testPtrPtr (int **x) {
}

void testArrayPtr (int (*a)[10]) {
}

void testArrayArg (int a[10]) {
}

typedef struct {
    int x;
    int y;
} simplePair;

typedef struct {
    simplePair parr[3];
} pairArray;

void testPairArray (pairArray *p) {
}

typedef struct node {
    int          data;
    struct node *next;
} node_t;

void testLinkedList (node_t *l) {
}

int * LOC(G) g;

void testAliasGlobal (int * LOC(G) h) GLOBAL (G) {
}

extern int REF(V > 0) testExtern OKEXTERN;

int getTestExtern () {
    return testExtern;
}

extern void * CONCRETE LOC(L) START NONNULL SIZE(sz) pmr_malloc (int REF(V >= 0) sz) OKEXTERN;

void testMalloc () {
    int *p = (int *) pmr_malloc (sizeof(int));
}

void globalWithoutVar (int * LOC(F) f) GLOBAL(F) {
}

typedef void foo () GLOBAL(G);

// Causes a global store not closed error, but that's surely not the fault
// of typespec.
// foo *p;

void withFunPtrArgument (int plusIt (int *, char *)) {
}

void globalFunPtrArgument (void (* LOC(K) k) ()) GLOBAL(K) {
}

void nestedFunPtrs (int applySomething (int foo (int))) {
}
