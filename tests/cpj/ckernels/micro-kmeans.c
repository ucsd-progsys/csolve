#include <cpj.h>
#include <stdlib.h>

extern
     int REF(V = sz2)
        * START NONNULL SIZE(4 * sz1) ARRAY
     mallocFloatMatrix (size_t REF(V > 0) sz1, size_t REF(V > 0) sz2)
  OKEXTERN;

void
normal_exec (int                              nfeatures,
             int * ARRAY START feature)
{
    return;
}

void main(void)
{
  int nfeatures = nondetpos();
  int npoints   = nondetpos();
  int* feature  = mallocFloatMatrix(npoints, nfeatures);

  normal_exec(nfeatures, feature);

  return;
}
