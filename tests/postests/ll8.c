#include <stdlib.h>

typedef struct node {
  struct node *next;
} node_t;

node_t *make_list(){
  node_t *root;
  node_t *tmp;

  root = 0;
  while(nondet()){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->next = root;
    root      = tmp;
  }
  return root;
}

void main(){
  node_t *root = make_list();

  if (root != (node_t *) 0){
    if (root->next != (node_t *) 0)
      csolve_assert(root->next != (node_t *) 0);
  }
    /*
  for(node_t *tmp = root; tmp != (node_t*) 0; tmp = tmp->next){
    if (tmp->next != (node_t*) 0){
      csolve_assert(tmp->next != (node_t*) 0);
    }
  }
  */

  return; 
}
