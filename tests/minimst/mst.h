/* For copyright information, see olden_v1.0/COPYRIGHT */

#ifdef SS_PLAIN
#include <stdlib.h>
#endif // SS_PLAIN

#ifndef DEPUTY
#define NTS
#define SAFE
#define FAT
#endif

#define MAXPROC 1

typedef struct vert_st {
  struct vert_st *next;
} *Vertex;

typedef struct graph_st {
    // #ifdef DEPUTY
  int numvert;
    // #endif
  Vertex /* COUNT(numvert) */ vlist;
} *Graph;

Graph MakeGraph(int numvert);
