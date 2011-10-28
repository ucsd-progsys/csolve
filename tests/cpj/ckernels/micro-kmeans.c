#include <cpj.h>
#include <stdlib.h>

extern
  float * START NONNULL SIZE(4 * sz2) ARRAY
        * START NONNULL SIZE(4 * sz1) ARRAY
     mallocFloatMatrix (size_t REF(V > 0) sz1, size_t REF(V > 0) sz2)
  OKEXTERN;

void
normal_exec (int                              nfeatures,
             float* ARRAY START * ARRAY START feature)
{
    return;
}

void main(void)
{
  int nfeatures   = nondetpos();
  int npoints     = nondetpos();
  float** feature = mallocFloatMatrix(npoints, nfeatures);

  normal_exec(nfeatures, feature);

  return;
}
