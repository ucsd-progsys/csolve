//! run-with --nop
// Scratchpad for testing types-as-specs

#define REF(p)   __attribute__ ((lcc_predicate (#p)))
#define LOC(l)   __attribute__ ((lcc_sloc (#l)))
#define GLOC(l)  __attribute__ ((lcc_gsloc (#l)))
#define CONCRETE __attribute__ ((lcc_concrete))
#define OKEXTERN __attribute__ ((lcc_extern_ok))

extern void pmr_assert (int REF(V != 0)) OKEXTERN;

extern void assert (int REF(V != 0)) OKEXTERN;

int *globalPointer;

int globalArray[10];

int REF(V > x) test (int x, int REF(&& [V > x; V >= 0]) y) {
    pmr_assert (y > x);
    pmr_assert (y >= 0);

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

int * GLOC(G) g;

void testAliasGlobal (int * GLOC(G) h) {
}

extern int REF(V > 0) testExtern OKEXTERN;

int getTestExtern () {
    return testExtern;
}

extern void * CONCRETE LOC(L) pmr_malloc (int REF(V >= 0)) OKEXTERN;

void testMalloc () {
    int *p = (int *) pmr_malloc (sizeof(int));
}
