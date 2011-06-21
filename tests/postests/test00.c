
#pragma lcc (include, "../tests.spec")

extern char* malloc(int);

int bar(int y){
  return 0;
}

int *foo(int *x, int *__attribute__((array)) y){
  return x;
}

void main(){
  int x;
  int a;
  int b;

  x = nondet();


  if (x < 0) {
    x = -x;
  }

  a = (x >= 0);
  
  a = 999;

  test_assert(x >= 0);

  assert(x >= 0);

  return; 
}
