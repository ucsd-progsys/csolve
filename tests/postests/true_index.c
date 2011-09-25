//! run with --manual

#include <stdlib.h>

struct vert_st {
  int mindist;
  int *boo;
};

typedef struct vert_st *Vertex;

struct graph_st {
  Vertex __attribute__((array)) vlist;
};

typedef struct graph_st *Graph;

void main(){
  Graph retval;
  Vertex vf;
  int i;
  int *junk;

  junk 		= (int *) malloc(4);

  i 		= nondetnn(); //crashes with nondet
  retval 	= (struct graph_st *) malloc(sizeof(*retval));
 
  retval->vlist = (Vertex __attribute__((array))) malloc(100 * sizeof(*vf));
  vf            = retval->vlist + i;
  vf->mindist   = 999;
  vf->boo       = junk;
}
