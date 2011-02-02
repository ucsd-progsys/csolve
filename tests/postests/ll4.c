extern char* malloc(int);

typedef struct node {
  int data;
  struct node *next;
} node_t;

node_t *main(){
  node_t *root;
  node_t *tmp;
  int i,n;

  n = 100;

  root = 0;

  for(i = 0; i < n; i++){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->data = i;
    tmp->next = root;
    root      = tmp;
  }

//  assert(0); //SANITY

  n = 92;
  return root;
}
