/* =============================================================================
 *
 * kmeans-mini.c
 * -- Adapted from the Stanford STAMP project
 * -- Skeleton implementation of normal k-means clustering algorithm
 *
 * =============================================================================
 */

#include <cpj.h>

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>

LCC_EFFECT(ACCUMULATE)
LCC_EFFECTS_COMMUTE(ACCUMULATE, ACCUMULATE)

typedef struct args {
    int REF(V > 0)                    nfeatures;
    int REF(V > 0)                    npoints;
    int REF(V <= npoints) REF(V > 0)  nclusters;
    int*   START ARRAY SIZE(4*npoints) membership;
    float* START ARRAY SIZE(4*nfeatures)
         * START ARRAY SIZE(4*npoints)    feature;  // [npoints][nfeatures]
    float* START ARRAY SIZE(4*nfeatures)
         * START ARRAY SIZE(4*nclusters)  clusters;            // [nclusters][nfeatures]
    int*   START ARRAY SIZE(4*nclusters) new_centers_len;
    float* START ARRAY SIZE(4*nfeatures) 
         * START ARRAY SIZE(4*nclusters) new_centers;
} args_t;

extern //atomic
  void accumulator(float delta,
                   int REF(V >= 0) REF(V < nclusters) index,
                   int REF(V >= 0) REF(V < npoints)   i,
                   int REF(V > 0)                     nfeatures,
                   int REF(V > 0)                     npoints,
                   int REF(V > 0) REF(V <= npoints)   nclusters,
                   float* ARRAY START SIZE(4*nfeatures)
                        * ARRAY START SIZE(4*npoints) feature,
                   int* ARRAY START SIZE(4*nclusters) new_centers_len,
                   float* ARRAY START SIZE(4*nfeatures)
                        * ARRAY START SIZE(4*nclusters) new_centers,
                   float* global_delta)
////{
////  /* Update new cluster centers : sum of objects located within */
////    args->new_centers_len[index]++;
////
////    for (int j = 0; j < args->nfeatures; j++)
////        args->new_centers[index][j] = args->new_centers[index][j] + args->feature[i][j];
////
////    global_delta += delta;
////}
////EFFECT(L, ACCUMULATE = 1)
////EFFECT(J, ACCUMULATE = 1)
////EFFECT(K, ACCUMULATE = 1)
////EFFECT(G, ACCUMULATE = 1)
OKEXTERN;

static void
work (int REF(V >= 0) REF(V < (DEREF([args + 4]):int))  i, args_t * args)
{
    int                              nfeatures       = args->nfeatures;
    int                              npoints         = args->npoints;
    int                              nclusters       = args->nclusters;
    float* ARRAY START * ARRAY START feature         = args->feature;
    int* ARRAY START                 membership      = args->membership;
    float* ARRAY START * ARRAY START clusters        = args->clusters;
    int* ARRAY START                 new_centers_len = args->new_centers_len;
    float* ARRAY START * ARRAY START new_centers     = args->new_centers;
    float delta = 0.0;
    int index;
    int j;

    //index = find_nearest_point(...);
    index = nondetrange(1, nclusters);

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
normal_exec (int REF(V > 0)                   nfeatures,
             int REF(V > 0)                   npoints,
             int REF(V <= npoints) REF(V > 0) nclusters,
             float                            threshold,
             float* ARRAY VALIDPTR START SIZE(4*nfeatures)
                  * ARRAY VALIDPTR START SIZE(4*npoints) feature,      /* in: [npoints][nfeatures] */
               int* ARRAY VALIDPTR START SIZE(4*npoints) membership)
{
    int i;
    int j;
    int loop = 0;
    int* ARRAY new_centers_len; /* [nclusters]: no. of points in each cluster */
    float delta, * global_delta;
    float* ARRAY * ARRAY  clusters;      /* out: [nclusters][nfeatures] */
    float* ARRAY * ARRAY  new_centers;   /* [nclusters][nfeatures] */
    args_t * args = NULL;

    global_delta = malloc(sizeof(float));

//    /* Allocate space for returning variable clusters[] */
    clusters = mallocFloatMatrix (nclusters, nfeatures);

//    /* clusters = (float**) malloc(nclusters * sizeof(float*)); */
//    /* for (i = 0; i < nclusters; i++) */
//    /*   clusters[i] = malloc(nfeatures * sizeof(float)); */
//
//    /* Randomly pick cluster centers */

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

//    /* /\* */
//    /*  * Need to initialize new_centers_len and new_centers[0] to all 0. */
//    /*  * Allocate clusters on different cache lines to reduce false sharing. */
//    /*  *\/ */
//    /* { */
//    /*     int cluster_size = sizeof(int) + sizeof(float) * nfeatures; */
//    /*     const int cacheLineSize = 32; */
//    /*     cluster_size += (cacheLineSize-1) - ((cluster_size-1) % cacheLineSize); */
//
//    /*     new_centers_len = (int**) malloc(nclusters * sizeof(int*)); */
//    /*     new_centers = (float**)  */
//
//    /*     for (i = 0; i < nclusters; i++) { */
//    /*         new_centers_len[i] = (int*)   malloc(cluster_size + sizeof(int)); */
//    /*     } */
//    /* } */

    //DO-WHILE LOOP PEEL
    delta = 0.0;

    args = (args_t*) malloc(sizeof(args_t));
    args->nfeatures       = nfeatures;
    args->npoints         = npoints;
    args->nclusters       = nclusters;
    args->membership      = membership;
    args->feature         = feature;
    args->clusters        = clusters;
    args->new_centers_len = new_centers_len;
    args->new_centers     = new_centers;

    *global_delta = delta;

    foreach (i, 0, npoints)
//    for (i = 0; i < npoints; i++)
      work(i, args);
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

// we can't reassign these because they need to be final
// although i have no idea why they're being re-assigned because they are final
//        args->nfeatures       = nfeatures;
//        args->npoints         = npoints;
//        args->nclusters       = nclusters;
        args->feature         = feature;
        args->membership      = membership;
        args->clusters        = clusters;
        args->new_centers_len = new_centers_len;
        args->new_centers     = new_centers;

        *global_delta = delta;

        foreach (i, 0, npoints)
//        for (i = 0; i < npoints; i++)
          work(i, args);
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

    /* free(alloc_memory); */
    free(new_centers);
    free(new_centers_len);

    return clusters;
}

int main2(int REF(v > 0) nfeatures, int REF(v > 0) npoints)
{
  assert(sizeof(float) == sizeof(int) == sizeof(void*) == 4);

  int nclusters   = nondetrange(1, npoints + 1);
  float threshold;
  float** feature = mallocFloatMatrix(npoints, nfeatures);
  int* membership = malloc(npoints * sizeof(int));

  normal_exec(nfeatures, npoints, nclusters, threshold, feature, membership);

  return 0;
}

int main(void)
{
  int nfeatures = nondetpos();
  int npoints = nondetpos();
  return main2(nfeatures, npoints);
}
