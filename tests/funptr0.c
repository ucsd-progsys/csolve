//#include <stdio.h>

int pinc(int a, int *x){
  *x ++;
  return 0;
}

int inc(int x){
  return x+1;
}

int wrap(int z, int (*f)(int a)){
  return (*f)(z);
}

void main(){
  int x,y;
  
  x = nondet();
  //x = 100; 
  
  y = wrap(x, &inc);
  
  assert(y >= x);
  //printf("hello world: x = %d, y = %d \n", x, y);
  
  return;
}
