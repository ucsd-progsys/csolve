extern char* malloc(int);

typedef struct node {
  int foobar;
  int data;
  struct node *next;
} node_t;

void main(int n){
  node_t *root;
  int i,y;

  root = (node_t *) malloc(sizeof(node_t));
 
  i = 0;
  root->data = i;
  
  while (nondet()){
    i++;
    root->data = i;
  }

  //assert(root->data >= 0);
  //assert(root->data >= i);

}
