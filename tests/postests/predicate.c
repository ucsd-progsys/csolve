//! run-with --nop
// Scratchpad for testing types-as-specs

#define REF(p) __attribute__ ((lcc_predicate (#p)))
#define LOC(l) __attribute__ ((lcc_sloc (#l)))

int REF(V > x) test (int x, int REF(&& [V > x; V >= 0]) y) {
    assert (y > x);
    assert (y >= 0);

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
