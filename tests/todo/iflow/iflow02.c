#include <csolve.h>

#include <stdlib.h>


int *global;

int *foo(int *x) {
  int *y = x;
  return y;
}

void bar(int *x) {
  gloabl = x;
}

void test01(int *x) {
  int *y;
  y = foo(x);
}

void test02(int *x) {
  int *y;
  if (g) {
    foo(x); //does foo need to know that it was called in the context of g?
            //i.e. if it updates a global variable?
  }
}

void test03(int *x) {
  int *y;
  if (g) {
    bar(x); //does bar need to know that it was called in the context of g?
            //i.e. the tags of //global should include the tags of g. But
            //are these even in scope now? The tags of global might be impossible to
            //reason about?
  }
}
