extern char* malloc(int);



typedef int ** intpp;


int **make_ptr(){
  int **t;
  t = (int **)malloc(sizeof(int *));
  return t;
}

void set_ptr(int **p, int v){
  int *x;

   x = (int *)malloc(sizeof(int));
  *x = v;
  *p = x;

  return;
}

void main(){
  int **t;
  int *t1;
  int t2;

  t = make_ptr();
  validptr(t);	//should succeed
  set_ptr(t, 10);
  
  validptr(t);	//should succeed
  t1 = *t;
  validptr(t1); //should fail
  t2 = *t1;

  assert(0 <= t2);

  return; 
}
