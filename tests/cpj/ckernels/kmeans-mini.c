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
    int REF(V > 0)                    nfeatures;
    int REF(V > 0)                    npoints;
    int REF(V <= npoints)             nclusters;
    int*   START ARRAY SIZE(4*npoints)    membership;
    float* START ARRAY SIZE(4*nfeatures)
         * START ARRAY SIZE(4*npoints)    feature;  // [npoints][nfeatures]
    float* START ARRAY SIZE(4*nfeatures)
         * START ARRAY SIZE(4*nclusters)  clusters;            // [nclusters][nfeatures]
    int*   START ARRAY SIZE(4*nclusters)  new_centers_len;
    float* START ARRAY SIZE(4*nfeatures)
         * START ARRAY SIZE(4*nclusters)  new_centers;
} args_t;

float REF(true) global_delta;


//static void accum_new_centers(int index, int i)
//{
//  { atomic
//    
//  }
//}
//
//
//  { atomic
//
//  }
//}


static void
work (int i, args_t * args)
{
//    float* ARRAY START * ARRAY START feature         = args->feature;
//    int                              nfeatures       = args->nfeatures;
//    int                              npoints         = args->npoints;
//    int                              nclusters       = args->nclusters;
//    int* ARRAY START                 membership      = args->membership;
//    float* ARRAY START * ARRAY START clusters        = args->clusters;
//    int* ARRAY START                 new_centers_len = args->new_centers_len;
//    float* ARRAY START * ARRAY START new_centers     = args->new_centers;
//    float delta = 0.0;
//    int index;
//    int i;
//    int j;

//    index = nondetrange(0, nclusters);

    /*
     * If membership changes, increase delta by 1.
     * membership[i] cannot be changed by other threads
     */
//    if (membership[i] != index) {
//        delta += 1.0;
//    }

//    /* Assign the membership to object i */
//    /* membership[i] can't be changed by other thread */
//    membership[i] = -1;
//
//    /* Update new cluster centers : sum of objects located within */
//    new_centers_len[index] = new_centers_len[index] + 1;
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
}

//float* ARRAY VALIDPTR START * ARRAY VALIDPTR START
void
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
    int* ARRAY new_centers_len; /* [nclusters]: no. of points in each cluster */
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
//

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
//
//

    global_delta = delta;

    //foreach (i, 0, npoints)
    for (i = 0; i < npoints; i++);
//      work(i, args);
    //endfor

//    delta = global_delta;
//
//    /* Replace old cluster centers with new_centers */
//    for (i = 0; i < nclusters; i++) {
//        for (j = 0; j < nfeatures; j++) {
//            if (new_centers_len[i] > 0) {
//                clusters[i][j] = new_centers[i][j] / new_centers_len[i];
//            }
//            new_centers[i][j] = 0.0;   /* set back to 0 */
//        }
//        new_centers_len[i] = 0;   /* set back to 0 */
//    }

//    delta /= npoints;

//    while ((delta > threshold) && (loop++ < 500))
//    {
//        delta = 0.0;
//
//        args = (args_t*) malloc(sizeof(args_t));
//        args->feature         = feature;
//        args->nfeatures       = nfeatures;
//        args->npoints         = npoints;
//        args->nclusters       = nclusters;
//        args->membership      = membership;
//        args->clusters        = clusters;
//        args->new_centers_len = new_centers_len;
//        args->new_centers     = new_centers;
//
//        global_delta = delta;
//
//        //foreach (i, 0, npoints)
//        for (i = 0; i < npoints; i++)
//          work(i, args);
//        //endfor
//
//        delta = global_delta;
//
//        /* Replace old cluster centers with new_centers */
//        for (i = 0; i < nclusters; i++) {
//            for (j = 0; j < nfeatures; j++) {
//                if (new_centers_len[i] > 0) {
//                    clusters[i][j] = new_centers[i][j] / new_centers_len[i];
//                }
//                new_centers[i][j] = 0.0;   /* set back to 0 */
//            }
//            new_centers_len[i] = 0;   /* set back to 0 */
//        }
//
//        delta /= npoints;
//    }

    /* free(alloc_memory); */
//    free(new_centers);
//    free(new_centers_len);
//
//    return clusters;
}

int main2(int REF(v > 0) nfeatures, int REF(v > 0) npoints)
{
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
