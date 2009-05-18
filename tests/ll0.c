extern char* malloc(int);

typedef struct node {
  int x;
  struct node *next;
} node_t;

void main(int n){
  node_t *root;
  node_t *tmp;
  int ctr;

  root = NULL;
  ctr = 0;

  for(int i=0; i++; i<n){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->x    = i;
    tmp->next = root;
    root      = tmp;
  }

  for(tmp = root; tmp != NULL; tmp = tmp->next){
    assert(tmp->x >= 0);
  }
}
