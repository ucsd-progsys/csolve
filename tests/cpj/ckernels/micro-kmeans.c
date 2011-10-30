#include <cpj.h>
#include <stdlib.h>

#define NFEATURES (DEREF([args]):int)

typedef struct args {
    int REF(V > 0) nfeatures;
    float* START VALIDPTR ARRAY SIZE_GE(4*NFEATURES)
         * START VALIDPTR ARRAY ROOM_FOR(float *) clusters;
} args_t;

extern void accumulator(args_t * args) OKEXTERN;

void check (args_t *args) { }

// It seems at first that the deref is getting wiped because the store has to
// be WF w.r.t. the env at the beginning of the function, but args only comes
// into scope at the middle of the function, so no quals in the store can
// deref it.
void main2 (int REF(V > 0) nfeatures, args_t *args) {
    float* ARRAY * clusters;

    clusters = mallocFloatMatrix (1, nfeatures);
    for (int j = 0; j < nfeatures; j++) {
      (*clusters)[j] = nondet ();
    }

    args->nfeatures = nfeatures;
    args->clusters  = clusters;

    check (args);

    // Safe if this line is commented
    accumulator (args);
}

void main (int REF(V > 0) nfeatures) CHECK_TYPE {
    args_t * args = (args_t*) malloc(sizeof(args_t));
    main2 (nfeatures, args);
}
