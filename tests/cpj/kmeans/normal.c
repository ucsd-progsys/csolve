/* =============================================================================
 *
 * normal.c
 * -- Adapted from the Stanford STAMP project
 * -- Implementation of normal k-means clustering algorithm
 *
 * =============================================================================
 */
#include <cpj.h>

//#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include "common.h"
#include "normal.h"
#include "util.h"


double global_time = 0.0;
float global_delta;

/* =============================================================================
 * work
 * =============================================================================
 */
void
work (float *ARRAY *ARRAY START feature,
      int   *ARRAY membership,
      float *ARRAY *ARRAY START clusters,
      int   *ARRAY START new_centers_len,
      float *ARRAY *ARRAY START new_centers,
      int nfeatures,
      int npoints,
      int nclusters,
      int i)
{
    csolve_assert(0 <= i);
    csolve_assert(i < npoints);
    csolve_assert(clusters);
    
    float delta = 0.0;
    int index;
    int j;
    index = 0;
    index = common_findNearestPoint(feature[i],
                                    nfeatures,
                                    clusters,
                                    nclusters);
    csolve_assert(0 <= index);
    csolve_assert(index < nclusters);

    /* If membership changes, increase delta by 1. */
    /* membership[i] cannot be changed by other threads */
    if (membership[i] != index) {
        delta += 1.0;
    }

    /* Assign the membership to object i */
    /* membership[i] can't be changed by other thread */
    membership[i] = index;
   
    accumulator(delta, index, i, nfeatures, npoints, nclusters,
    		feature, new_centers_len, new_centers, &global_delta);
}
/* =============================================================================
 * normal_exec
 * =============================================================================
 */
float ** 
normal_exec (//int       nthreads,
             float **     feature,    /* in: [npoints][nfeatures] */
             int          nfeatures,
             int          npoints,
             int          nclusters,
             float        threshold,
             int *        membership)
//             random_t* randomPtr) /* out: [npoints] */
    CHECK_TYPE
{
    int i;
    int j;
    int loop = 0;
    float delta;
    
    csolve_assert(nfeatures > 0);
    float * ARRAY * ARRAY clusters    = init_float2d(nclusters, nfeatures);
    float * ARRAY * ARRAY new_centers = init_float2d(nclusters, nfeatures);
    int * ARRAY       new_centers_len = (int*) malloc(nclusters * sizeof(int));
  
    for (i = 0; i < nclusters; i++) {
      new_centers_len[i] = 0; 
    }
    
    /* Randomly pick cluster centers */
    for (i = 0; i < nclusters; i++) {
      int n = nondetrange(0, npoints); 
      for (j = 0; j < nfeatures; j++) {
        float foo = feature[n][j];
        clusters[i][j] = foo;
      }
    }
  
    foreach (i, 0, npoints)
      membership[i] = -1;
    endfor 
    
    do {
        delta = 0.0;
        global_delta = delta;
        foreach (i, 0, npoints)
	  work(feature, 
	       membership, 
	       clusters, 
	       new_centers_len, 
	       new_centers,
	       nfeatures,
	       npoints,
	       nclusters,
	       i);
	endfor
        delta = global_delta;

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

    } while ((delta > threshold) && (loop++ < 500));

//    free(alloc_memory);
//    free(new_centers);
//    free(new_centers_len);

    return clusters;
}

/* =============================================================================
 *
 * End of normal.c
 *
 * =============================================================================
 */
