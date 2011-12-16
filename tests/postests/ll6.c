//! run with --manual

#include <stdlib.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t *new_node(){
  node_t *n = (node_t *) malloc(sizeof(*n));
  validptr(n);
  n-> data = 0;
  n-> next = n ;
  return n;
}

node_t *foo(int n){
  node_t *root;
  node_t *tmp;
  int i;

  root = new_node();

  for(i = 0; i < n; i++){
    tmp        = new_node();
    tmp->data  = i;
    tmp->next  = root->next;
    root->next = tmp;
  }

  return root;
}

int bar(int n){
  node_t *root;
  node_t *tmp;
  
  root = foo(n);

  for(tmp = root->next; tmp != root; tmp = tmp->next){
    validptr(tmp);
    csolve_assert(tmp->data >= 0);
  }
 
  return 0;
}

int main(){
  int n = nondet();
  bar(n);
  return 0;
}

