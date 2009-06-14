extern char* malloc(int);

typedef struct node {
  int foobar;
  int data;
  struct node *next;
} node_t;

int main(){
  node_t *root;
  int i,y;

  root = (node_t *) malloc(sizeof(node_t));
 
  i = 0;
  root->data = i;
  
  while (nondet()){
    i++;
    root->data = i;
  }

  y = root->data;
  assert(y >= 0);
  //assert(y >= i);  store-scope-issue

  return 0;
   
}
