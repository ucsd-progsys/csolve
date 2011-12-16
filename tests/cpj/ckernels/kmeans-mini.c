/* =============================================================================
 *
 * kmeans-mini.c
 * -- Adapted from the Stanford STAMP project
 * -- Skeleton implementation of normal k-means clustering algorithm
 *
 * =============================================================================
 */

/*
 * MODIFICATIONS/CHEATS:
 *
 * - externed accumulator() function so that we could declare its
 *    effect etc.
 *
 * - Originally used a structure to pass around arguments. Changed
 *   functions to accept what used to be the members of the struct as
 *   arguments.
 * 
 * - Pass array sizes around so that they can be in scope to constraint size 
 *
 * - Thread global variable global_delta through functions so that we
 *   can name its location.
 *
 * - main()/main2() wrappers so that we can properly constraint the
 *   size of the arrays that will be used.
 *
 * - custom allocators for feature, new_centers_len, new_centers.
 */

#include <cpj.h>

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>

CSOLVE_EFFECT(ACCUMULATE)
CSOLVE_EFFECTS_COMMUTE(ACCUMULATE, ACCUMULATE)

extern //atomic
  void accumulator(float delta,
                   int REF(V >= 0) REF(V < nclusters) index,
                   int REF(V >= 0) REF(V < npoints)   i,
                   int REF(V > 0)                     nfeatures,
                   int REF(V > 0)                     npoints,
                   int REF(V > 0) REF(V <= npoints)   nclusters,
                   float* ARRAY NONNULL START SIZE(4*nfeatures) LOC(A)
                        * ARRAY NONNULL START SIZE(4*npoints)   LOC(B) feature,
                   int  * ARRAY NONNULL START SIZE(4*nclusters) LOC(C) new_centers_len,
                   float* ARRAY NONNULL START SIZE(4*nfeatures) LOC(D)
                        * ARRAY NONNULL START SIZE(4*nclusters) LOC(E) new_centers,
                   float* LOC(F) global_delta)
EFFECT(A, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(B, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(C, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
EFFECT(D, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
EFFECT(E, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(F, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
OKEXTERN;

static void
work (int i,
      /* ghost parameters for predicate scoping issue with args */
      int nfeatures, int npoints, int nclusters,
      float* ARRAY START * ARRAY START feature,
      int* ARRAY START new_centers_len,
      float* ARRAY START * ARRAY START new_centers,
      int*  ARRAY START membership,
      /* we have to thread the global because there's no other way to name its location */
      float* global_delta)
{
    float delta = 0.0;
    int index;
    int j;

    //index = find_nearest_point(...);
    index = nondetrange(0, nclusters);

    /*
     * If membership changes, increase delta by 1.
     * membership[i] cannot be changed by other threads
     */
    if (membership[i] != index) {
        delta += 1.0;
    }

    /* Assign the membership to object i */
    /* membership[i] can't be changed by other thread */
    membership[i] = index;

    accumulator(delta, index, i,
                nfeatures,
                npoints,
                nclusters,
                feature,
                new_centers_len,
                new_centers,
                global_delta);
}

float* ARRAY VALIDPTR START * ARRAY VALIDPTR START
normal_exec (int nfeatures, int npoints, int nclusters,
	     float threshold,
             float* ARRAY START * ARRAY START feature, /* in: [npoints][nfeatures] */
	     int* ARRAY START membership)
{
    int i;
    int j;
    int loop = 0;
    int *new_centers_len; /* [nclusters]: no. of points in each cluster */
    float delta, *global_delta;
    float **clusters;      /* out: [nclusters][nfeatures] */
    float **new_centers;   /* [nclusters][nfeatures] */

    global_delta = malloc(sizeof(float));

       /* Allocate space for returning variable clusters[] */
    clusters = mallocFloatMatrix (nclusters, nfeatures);

   /* Randomly pick cluster centers */

    for (i = 0; i < nclusters; i++) {
        int n = nondetrange(0, npoints);
        for (j = 0; j < nfeatures; j++) {
          clusters[i][j] = feature[n][j];
        }
    }

    foreach (i, 0, npoints)
      membership[i] = -1;
    endfor

    new_centers     = mallocFloatMatrix(nclusters, nfeatures);
    new_centers_len = callocInt(nclusters);

    //DO-WHILE LOOP PEEL
    delta = 0.0;

    *global_delta = delta;

    foreach (i, 0, npoints)
      work(i, nfeatures, npoints, nclusters, feature,
           new_centers_len, new_centers, membership, global_delta);
    endfor

    delta = *global_delta;

    /* Replace old cluster centers with new_centers */
    for (i = 0; i < nclusters; i++) {
        for (j = 0; j < nfeatures; j++) {
            if (new_centers_len[i] > 0) {
                clusters[i][j] = new_centers[i][j] / new_centers_len[i];
            }
            new_centers[i][j] = 0.0;   /* set back to 0 */
        }
        new_centers_len[i] = 0;   /* set back to 0 */
    }

    delta /= npoints;

    while ((delta > threshold) && (loop++ < 500))
    {
        delta = 0.0;

        *global_delta = delta;

        foreach (i, 0, npoints)
          work(i, nfeatures, npoints, nclusters, feature,
               new_centers_len, new_centers, membership, global_delta);
        endfor

        delta = *global_delta;

        /* Replace old cluster centers with new_centers */
        for (i = 0; i < nclusters; i++) {
            for (j = 0; j < nfeatures; j++) {
                if (new_centers_len[i] > 0) {
                    clusters[i][j] = new_centers[i][j] / new_centers_len[i];
                }
                new_centers[i][j] = 0.0;   /* set back to 0 */
            }
            new_centers_len[i] = 0;   /* set back to 0 */
        }

        delta /= npoints;
    }

    free(new_centers);
    free(new_centers_len);

    return clusters;
}

int main2(int nfeatures, int npoints)
{
  int nclusters    = nondetrange(1, npoints + 1);
  float threshold;
  float** feature  = mallocFloatMatrix(npoints, nfeatures);
  int* membership  = malloc(npoints * sizeof(int));

  normal_exec(nfeatures, npoints, nclusters, threshold, feature, membership);

  return 0;
}

int main(void)
{
  int nfeatures = nondetpos();
  int npoints = nondetpos();
  return main2(nfeatures, npoints);
}
