extern char* malloc(int);

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t *foo(int n){
  node_t *root;
  node_t *tmp;
  int i;

  root = 0;

  for(i = 0; i < n; i++){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->data = i;
    tmp->next = root;
    root      = tmp;
  }

  return root;
}

int bar(int n){
  node_t *root;
  node_t *tmp;
  
  root = foo(n);

  for(tmp = root; tmp != (node_t*) 0; tmp = tmp->next){
    assert(tmp->data >= 0);
    assert(tmp->data < n);
    //assert(tmp->data < 100); UNSAFE
  }
 
  return 0;
}

int main(){
  int n; 
  
  n = nondet();
  
  bar(n);
  
  return 0;
}



