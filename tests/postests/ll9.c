#include <stdlib.h>

typedef struct node {
  struct node *next;
} node_t;

void main(){
  node_t *root;
  node_t *tmp;

  root = 0;

  while(nondet()){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->next = root;
    root      = tmp;
  }

  for(tmp = root; tmp != (node_t*) 0; tmp = tmp->next){
    if (tmp->next != (node_t*) 0){
      csolve_assert(tmp->next != (node_t*) 0);
    }
  }
  
  return;

}
