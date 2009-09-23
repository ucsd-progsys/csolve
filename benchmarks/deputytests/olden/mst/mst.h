/* For copyright information, see olden_v1.0/COPYRIGHT */

#ifdef SS_PLAIN
#include <stdlib.h>
#endif // SS_PLAIN

#ifndef DEPUTY
#define NTS
#define SAFE
#define FAT
#endif

#define NULL 0

#include "hash.h"
#define MAXPROC 1

#ifndef SS_PLAIN
/* Amir: Get these out of here for SS, piggyback on the PLAIN definition */
#include "mem-ref.h"
#define NULL 0
#endif // SS_PLAIN

typedef struct vert_st {
  int mindist;
  struct vert_st *next;
  Hash edgehash;
  unsigned int padding;
} *Vertex;

typedef struct graph_st {
  Vertex COUNT(numvert) vlist;
#ifdef DEPUTY  
  int numvert;
#endif  
} *Graph;

Graph MakeGraph(int numvert);

#ifdef SS_PLAIN
#include <stdarg.h>
void chatting(char * NTS s, ...);
#endif // SS_PLAIN

