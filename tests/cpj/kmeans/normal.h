/* =============================================================================
 *
 * normal.h
 * -- Implementation of normal k-means clustering algorithm
 *
 * =============================================================================
 *
 * For the license of bayes/sort.h and bayes/sort.c, please see the header
 * of the files.
 * 
 * ------------------------------------------------------------------------
 * 
 * For the license of kmeans, please see kmeans/LICENSE.kmeans
 * 
 * ------------------------------------------------------------------------
 * 
 * For the license of ssca2, please see ssca2/COPYRIGHT
 * 
 * ------------------------------------------------------------------------
 * 
 * For the license of lib/mt19937ar.c and lib/mt19937ar.h, please see the
 * header of the files.
 * 
 * ------------------------------------------------------------------------
 * 
 * For the license of lib/rbtree.h and lib/rbtree.c, please see
 * lib/LEGALNOTICE.rbtree and lib/LICENSE.rbtree
 * 
 * ------------------------------------------------------------------------
 * 
 * Unless otherwise noted, the following license applies to STAMP files:
 * 
 * Copyright (c) 2007, Stanford University
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 * 
 *     * Neither the name of Stanford University nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY STANFORD UNIVERSITY ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL STANFORD UNIVERSITY BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 * =============================================================================
 */


#ifndef NORMAL_H
#define NORMAL_H 1

#include <csolve.h>
#include "common.h"
//#include "random.h"


extern double global_parallelTime;

#define FLOAT2D_LOC(x, y, l, m)				\
  float * ARRAY VALIDPTR SIZE_GE(4*y) LOC(m)		\
        * ARRAY START VALIDPTR SIZE_GE(4*x) LOC(l)
extern //atomic
  void accumulator(float delta,
                   int REF(V >= 0) REF(V < nclusters) index,
                   int REF(V >= 0) REF(V < npoints)   i,
                   int REF(V > 0)                     nfeatures,
                   int REF(V > 0)                     npoints,
                   int REF(V > 0)                     nclusters,
		   FLOAT2D_LOC(npoints, nfeatures, A, B) feature,
                   int  * ARRAY START SIZE_GE(4*nclusters) LOC(C) new_centers_len,
		   FLOAT2D_LOC(nclusters, nfeatures, D, E) new_centers,
                   float* LOC(F) global_delta)
EFFECT(A, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(B, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(C, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
EFFECT(D, &&[ACCUMULATE != 1; EREAD != 1; EWRITE != 1])
EFFECT(E, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
EFFECT(F, &&[ACCUMULATE = 1; EREAD != 1; EWRITE != 1])
OKEXTERN;

/* =============================================================================
 * normal_exec
 * =============================================================================
 */
//float* FLOATARR(nfeatures*nclusters)* START FLOATARR(nclusters)

FLOAT2D(nclusters, nfeatures)
normal_exec (//int       nthreads,
	     FLOAT2D(npoints, nfeatures)    feature, /*  in:[npoints][nfeatures] */ 
	     int    REF(V > 0)              nfeatures,
	     int    REF(V > 0)              npoints,
	     int    REF(V > 0)              nclusters,
         float  REF(V > 0)              threshold,
         int   *START FLOATARR(npoints) membership) OKEXTERN;
             //random_t* randomPtr); /* out: [npoints] */
#endif /* NORMAL_H */


/* =============================================================================
 *
 * End of normal.h
 *
 * =============================================================================
 */
