#include <stdlib.h>

struct vert_st {
  int mindist;
};

struct graph_st {
  struct vert_st * ARRAY vlist;
};

typedef struct graph_st *Graph;

Graph MakeGraph(int numvert){
  Graph retval;
  struct vert_st* vf;

  retval = (Graph) malloc(sizeof(*retval));
  retval->vlist = (struct vert_st *) malloc(numvert * sizeof(*vf));
  for (int i=0; i < numvert; i++){
    vf = retval->vlist + i;
    vf->mindist = 99;
  }
  return retval;
}

void main(){
  int n;
  Graph g;
  n = nondetpos();
  g = MakeGraph(n);
  validptr(g->vlist);
}
