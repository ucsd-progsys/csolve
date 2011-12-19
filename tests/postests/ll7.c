//! run with --notruekvars

#include <csolve.h>
#include <stdlib.h>

typedef struct node {
  int *data;
} node_t;

node_t *new_node(){
  node_t *n = (node_t *) malloc(sizeof(*n));
  n->data = (int *)0;
  return n;
}

node_t *foo(){
  node_t *root;
  int *x     = (int *) malloc(sizeof(*x));
  *x         = 5;
  root       = new_node();
  root->data = x; 
  return root;
}

void main(){
  node_t *n = foo();
  int    *x = n->data;
  int assm  = csolve_assume(x != (int *) 0);
  validptr(x);
  csolve_assert(*x >= 0);
  return;
}
