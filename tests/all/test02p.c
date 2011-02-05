extern char* malloc(int);

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);
  *r = 0;

  while (nondet()){
   *r = *r + 1;
  }

  y = *r;
  assert(y >= 0);
  
  return 0;
}
