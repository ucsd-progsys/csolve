extern char* malloc(int);

void main(){
  int *x;
  
  x = 0;

  if (nondet()) {
    x  = (int *) malloc(sizeof(*x));
  }

  return;
}
