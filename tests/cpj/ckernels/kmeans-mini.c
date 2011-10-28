/* =============================================================================
 *
 * normal.c
 * -- Adapted from the Stanford STAMP project
 * -- Implementation of normal k-means clustering algorithm
 *
 * =============================================================================
 */


#include <cpj.h>

#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>

typedef struct args {
    float* START ARRAY SIZE(nfeatures * FSZ)
         * START ARRAY SIZE(npoints * PSZ) feature;  // [npoints][nfeatures]
    int                               nfeatures;
    int                               npoints;
    int REF(V <= npoints)             nclusters;
    int* START ARRAY SIZE(npoints * ISZ)    membership;
    float* START ARRAY SIZE(nfeatures * FSZ)
         * START ARRAY SIZE(nclusters * PSZ) clusters;            // [nclusters][nfeatures]
    int** START ARRAY SIZE(nclusters * PSZ)  new_centers_len;
    float* START ARRAY SIZE(nfeatures * FSZ)
         * START ARRAY SIZE(nclusters * PSZ) new_centers;
} args_t;

float REF(true) global_delta;

/* extern void work (int i, args_t *args) OKEXTERN; */


/* =============================================================================
 * work
 * =============================================================================
 */
//static void
//work (int i, args_t * args)
//{
//    float ** feature         = args->feature;
//    int     nfeatures       = args->nfeatures;
//    int     npoints         = args->npoints;
//    int     nclusters       = args->nclusters;
//    int*    membership      = args->membership;
//    float*  *  clusters        = args->clusters;
//    int* *    new_centers_len = args->new_centers_len;
//    float*  *  new_centers     = args->new_centers;
//    float delta = 0.0;
//    int index;
//    int i;
//    int j;
//
//    /*
//     * If membership changes, increase delta by 1.
//     * membership[i] cannot be changed by other threads
//     */
//    if (membership[i] != index) {
//        delta += 1.0;
//    }
//
//    /* Assign the membership to object i */
//    /* membership[i] can't be changed by other thread */
//    membership[i] = -1;
//
//    /* Update new cluster centers : sum of objects located within */
//    *new_centers_len[index] = *new_centers_len[index] + 1;
//
//    //ACCUMULATE
//    { atomic
//      for (j = 0; j < nfeatures; j++)
//          new_centers[index][j] = new_centers[index][j] + feature[i][j];
//    } 
//
//    { atomic
//      global_delta = global_delta + delta;
//    }
//}

extern
  float * NONNULL SIZE(4 * sz2) START ARRAY
        * LOC(L) START NONNULL SIZE(4 * sz1) ARRAY
     mallocFloatMatrix (int REF(V > 0) sz1, int REF(V > 0) sz2)
  OKEXTERN;

extern
  int * NONNULL SIZE(4 * sz2) START ARRAY
      * LOC(L) START NONNULL SIZE(4 * sz1) ARRAY
     mallocIntMatrix (int REF(V > 0) sz1, int REF(V > 0) sz2)
  OKEXTERN;

//float* ARRAY VALIDPTR START * ARRAY VALIDPTR START
//normal_exec (float* ARRAY VALIDPTR START SIZE(nfeatures * FSZ)
//                  * ARRAY VALIDPTR START SIZE(npoints * PSZ) feature,      /* in: [npoints][nfeatures] */
//             int REF(V > 0)                   nfeatures,
//             int REF(V > 0)                   npoints,
//             int REF(V <= npoints) REF(V > 0) nclusters,
//             float                            threshold,
//             int* ARRAY VALIDPTR START SIZE(npoints * ISZ) membership)

float* ARRAY VALIDPTR START * ARRAY VALIDPTR START
normal_exec (int REF(V > 0)                   nfeatures,
             int REF(V > 0)                   npoints,
             int REF(V <= npoints) REF(V > 0) nclusters,
             float                            threshold,
             float* ARRAY VALIDPTR START SIZE(4*nfeatures)
                  * ARRAY VALIDPTR START SIZE(4*npoints) feature,      /* in: [npoints][nfeatures] */
               int* ARRAY VALIDPTR START SIZE(4*npoints) membership)
  //CHECK_TYPE
{
    int i;
    int j;
    int loop = 0;
    int** ARRAY new_centers_len; /* [nclusters]: no. of points in each cluster */
    float delta;
    float* ARRAY * ARRAY  clusters;      /* out: [nclusters][nfeatures] */
    float* ARRAY * ARRAY  new_centers;   /* [nclusters][nfeatures] */
    args_t * args = NULL;

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
          float * featuren = feature[n];
          float featurezzz = featuren[j];
          //            float *clustersi = clusters[i];
            //            LCC_ASSUME(clustersi != NULL);
          //clusters[i][j] = feature[n][j];
        }
    }
//
//    // foreach (i, 0, npoints)
//    for(i = 0; i < npoints; i++)
//        membership[i] = -1;
//    // endfor
//
//    new_centers     = mallocFloatMatrix(nclusters, nfeatures);
//    new_centers_len = malloc(sizeof (int) * nclusters);
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
//
////    do {
////        delta = 0.0;
////
////        args = (args_t*) malloc(sizeof(args_t));
////        args->feature         = feature;
////        args->nfeatures       = nfeatures;
////        args->npoints         = npoints;
////        args->nclusters       = nclusters;
////        args->membership      = membership;
////        args->clusters        = clusters;
////        args->new_centers_len = new_centers_len;
////        args->new_centers     = new_centers;
////
////        global_delta = delta;
////
////////        //foreach (i, 0, npoints)
//////        for (i = 0; i < npoints; i++)
//////          work(i, args);
////////        //endfor
////
////        delta = global_delta;
////
////        /* Replace old cluster centers with new_centers */
////        for (i = 0; i < nclusters; i++) {
////            for (j = 0; j < nfeatures; j++) {
////                if (new_centers_len[i] > 0) {
////                    clusters[i][j] = new_centers[i][j] / new_centers_len[i];
////                }
////                new_centers[i][j] = 0.0;   /* set back to 0 */
////            }
////            new_centers_len[i] = 0;   /* set back to 0 */
////        }
////
////        delta /= npoints;
////    } while ((delta > threshold) && (loop++ < 500));
//
//    /* free(alloc_memory); */
//    free(new_centers);
//    free(new_centers_len);
//
    return clusters;
}

int main(void)
{
  int nfeatures   = nondetpos();
  int npoints     = nondetpos();
  int nclusters   = nondetrange(1, npoints + 1);
  float threshold;
  float** feature = mallocFloatMatrix(npoints, nfeatures);
  int* membership = malloc(npoints * sizeof(int));

  normal_exec(nfeatures, npoints, nclusters, threshold, feature, membership);

  return 0;
}
