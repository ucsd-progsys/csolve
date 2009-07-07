extern char* malloc(int);

typedef struct node {
  int x;
  int y;
  struct node *next;
} node_t;

void main(/* int n */){
  node_t *root;
  node_t *tmp;

  root = (node_t*)0;

  for (int i=0; i < 100 /* n */; i++) {
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->x    = i;
    tmp->y    = i+1;
    tmp->next = root;
    root      = tmp;
  }

  for(tmp = root; tmp!=(node_t*)0; tmp = tmp->next){
    assert(tmp->x >= 0);
    assert(tmp->x < tmp->y);
    assert(tmp->x > tmp->y); //UNSAFE
    
  }
}
