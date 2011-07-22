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

/* void testpair (pair *p) { */
/*     assert (p->snd > p->fst); */
/* } */
