extern char* malloc(int);

typedef struct node {
  struct node *next;
} node_t;

void init(int **x){
  *x = (int *) malloc(sizeof(int));
  **x = 22;
  return;
}

void main(){
  int **y;
  
  y  = (int **) malloc(sizeof(int *));
  *y = (int *) 0;

  init(y);

  if (*y != (int *) 0){
    assert(**y >= 0);
  }
 
  return; 
}
