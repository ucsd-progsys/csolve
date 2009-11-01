extern char* malloc(int);

int bar(int y){
  return 0;
}

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
