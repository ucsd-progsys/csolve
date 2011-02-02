extern char* malloc(int);

typedef struct node {
  int data;
  struct node *next;
} node_t;

int foo(int n){
  node_t *root;
  node_t *tmp;
  int i;

  root = 0;
  
//  assert(0); //SANITY

  for(i = 0; i < n; i++){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->data = i;
    tmp->next = root;
    root      = tmp;
  }

//  assert(0); //SANITY

  for(tmp = root; tmp != (node_t*) 0; tmp = tmp->next){
    assert(tmp->data >= 0);
    assert(tmp->data < n);
    assert(tmp->data < 100);
//    assert(0); //SANITY
  }

return 0;
}

int main(){
  foo(100);
  return 0;
}
