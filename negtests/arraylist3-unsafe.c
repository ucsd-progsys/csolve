extern char* malloc(int);

struct foo {
  int data;
};

struct foo *__attribute__((array)) main(){
  struct foo *a;
  struct foo *b;

  int i,j;
  int n;

  n  = nondetpos();
  a  = (struct foo *) malloc(100 * sizeof(struct foo));
 
  i = n-1;
  while (i >= 0){
    b 	    = a + i;
    b->data = 999;
    i--;
  }

  for (j=0; j < 5; j++){
    b       = a + j;
    assert(b->data == 999);
  }

  return a;
}
