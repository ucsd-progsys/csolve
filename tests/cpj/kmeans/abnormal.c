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
#include "util.h"


double global_time = 0.0;

float global_delta;

typedef struct args {
  int  REF(V > 0)                    num;
  float *SIZE_GE(4*num) ARRAY *ARRAY feature;
} args_t;

/* =============================================================================
 * work
 * =============================================================================
 */
void work (args_t *args, int i, int npoints) CHECK_TYPE
{
}
