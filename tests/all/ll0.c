extern char* malloc(int);

typedef struct node {
  int data;
  struct node *next;
} node_t;

int main(){
  node_t *root;
  node_t *tmp;

  root = 0;

  for(int i=0; i < 1000; i++){
    tmp       = (node_t *) malloc(sizeof(node_t));
    tmp->data = i;
    tmp->next = root;
    root      = tmp;
  }

  for(tmp = root; tmp != (node_t*) 0; tmp = tmp->next){
    assert(tmp->data >= 0);
    //assert(tmp->data < n);
  }
  
  return 0;
}
