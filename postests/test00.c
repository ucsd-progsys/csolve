extern char* malloc(int);

void main(){
  int x;

  x = nondet();


  if (x < 0) {
    x = -x;
  }

  assert(x >= 0);

  return; 
}
