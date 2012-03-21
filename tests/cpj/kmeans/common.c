/* =============================================================================
 *
 * common.c
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

#include <csolve.h>

#include "common.h"


/* =============================================================================
 * common_euclidDist2
 * -- multi-dimensional spatial Euclid distance square
 * =============================================================================
 */
float
common_euclidDist2 (float* FTUPLE(numdims) pt1, 
                    float* FTUPLE(numdims) pt2, 
                    int    REF(V >= 0)     numdims) 
  CHECK_TYPE
{
    int i;
    float ans = 0.0F;

    for (i = 0; i < numdims; i++) {
        ans += (pt1[i] - pt2[i]) * (pt1[i] - pt2[i]);
    }

    return ans;
}


/* =============================================================================
 * common_findNearestPoint
 * =============================================================================
 */
int
common_findNearestPoint (float*  ARRAY START VALIDPTR SIZE_GE(nfeatures*4) pt,        /* [nfeatures] */
                         int     REF(V >= 0) nfeatures,
                         float* ARRAY START VALIDPTR SIZE_GE(nfeatures*4) 
                              * ARRAY START VALIDPTR SIZE_GE(npts*4) pts,       /* [npts][nfeatures] */
                         int     REF(V >= 0) npts)
  CHECK_TYPE
{
    int index = -1;
    int i;
    float max_dist = FLT_MAX;
    const float limit = 0.99999;

    /* Find the cluster center id with min distance to pt */
    for (i = 0; i < npts; i++) {
        float dist;
        dist = common_euclidDist2(pt, pts[i], nfeatures);  /* no need square root */
        if ((dist / max_dist) < limit) {
            max_dist = dist;
            index = i;
            if (max_dist == 0) {
                break;
            }
        }
    }

    return index;
}


/* =============================================================================
 *
 * End of common.c
 *
 * =============================================================================
 */
