#include <stdlib.h>

typedef struct node {
  struct node *next;
} node_t;

/*
void init(int **x){
  *x = (int *) malloc(sizeof(int));
  **x = 22;
  return;
}
*/

void main(){
  int **y;
  int *z;

  y  = (int **) malloc(sizeof(int *));
  
  //BAD
  *y = (int *) 0; 
  int t = (*y != 0);

  //GOOD
/*
  z  = (int *) 0;
  *y = z;
  int t = (*y != 0);
*/
  return; 
}
