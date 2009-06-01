extern char* malloc(int);

typedef struct node {
  int data;
  struct node *next;
} node_t;

void main(/* int n */){
  node_t *root;
  node_t *tmp;
  int ctr;

  root = 0;
  ctr = 0;

  for(int i=0; i < 100 /* n */; i++){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->data = i - 5;
    tmp->next = root;
    root      = tmp;
  }

  for(tmp = root; tmp != 0; tmp = tmp->next){
    assert(tmp->data >= 0);
    //assert(tmp->data < n);
  }
}
