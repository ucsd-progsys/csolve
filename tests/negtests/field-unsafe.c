#include <stdlib.h>

typedef struct node {
  int foobar;
  int data;
  struct node *next;
} node_t;

void main(int n){
  node_t *root;
  int i,y;

  root = (node_t *) malloc(sizeof(node_t));
 
  i = 0;
  root->data = i;
 
  while (nondet()){
    i--;
    root->data = i;
  }

  y = root->data;
  csolve_assert(y >= 0);
  //csolve_assert(y >= i);  store-scope-issue
}
