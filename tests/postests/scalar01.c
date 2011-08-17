#include <stdlib.h>

typedef struct node {
  int data;
  struct node *next;
} node_t;

int main(){
  node_t *root;
  node_t *tmp;

  tmp       = (node_t *) malloc(sizeof(node_t));
  
  root = 0;

  root = root + 2 ;

  return 0;
}
