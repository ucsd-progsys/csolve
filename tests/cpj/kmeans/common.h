/* =============================================================================
 *
 * common.h
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

#include <cpj.h>
#include <csolve.h>

#ifndef COMMON_H
#define COMMON_H 1


#ifndef FLT_MAX
#  define FLT_MAX 3.40282347e+38
#endif

CSOLVE_EFFECT(ACCUMULATE)
CSOLVE_EFFECTS_COMMUTE(ACCUMULATE, ACCUMULATE)

#define FLOATARR(n) ARRAY VALIDPTR SIZE_GE(4*n)
#define INTARR FLOATARR
#define FTUPLE(n) START FLOATARR(n)
#define NNFLOATARR(n) ARRAY NNVALIDPTR NNSIZE_GE(4*n)
#define NNINTARR NNFLOATARR
#define FLOAT2D(x, y) float * FLOATARR(y) * START FLOATARR(x)
#define NNFLOAT2D(x, y) float * FLOATARR(y) * NNSTART NNFLOATARR(x)

extern FLOAT2D(x, y) init_float2d(int REF(V > 0) x, int REF(V > 0) y) OKEXTERN;
extern void *copy_float2d( int REF(V > 0) x, int REF(V > 0) y, FLOAT2D(x, y) dest, FLOATARR(x * y) src) OKEXTERN;
/* =============================================================================
 * common_euclidDist2
 * -- multi-dimensional spatial Euclid distance square
 * =============================================================================
 */
float
common_euclidDist2 (float* FLOATARR(numdims) pt1,
                    float* FLOATARR(numdims) pt2, 
                    int    REF(V >= 0)     numdims); 


/* =============================================================================
 * common_findNearestPoint
 * =============================================================================
 */
int REF(|| [&& [(0 < V); (V < npts)];(V = 0)])
common_findNearestPoint(float* LOC(A) FLOATARR(nfeatures) pt, /* [nfeatures] */
                        int     REF(V >= 0) nfeatures,
                        float *LOC(B) FLOATARR(nfeatures) 
			      *LOC(C) FTUPLE(npts) pts, /* [npts][nfeatures] */
                        int     REF(V > 0) npts) 
     EFFECT(A, &&[ACCUMULATE != 1; EREAD = 1; EWRITE != 1])
     EFFECT(B, &&[ACCUMULATE != 1; EREAD = 1; EWRITE != 1])
     EFFECT(C, &&[ACCUMULATE != 1; EREAD = 1; EWRITE != 1])
OKEXTERN;

#endif /* COMMON_H */


/* =============================================================================
 *
 * End of common.h
 *
 * =============================================================================
 */
