extern char* malloc(int);

int *foo(int *x, int *__attribute__((array)) y){
  return x;
}

void main(){
  int x;

  x = nondet();


  if (x < 0) {
    x = -x;
  }

  assert(x >= 0);

  return; 
}
