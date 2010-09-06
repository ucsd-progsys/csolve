
int inc(int x){
  return x+1;
}

int wrap(int arg, int (*f)(int)){
  return (*f)(arg);
}

void main(){
  int x,y;
  x = nondet();
  y = wrap(x, &inc);
  assert(y >= x);
  return;
}
