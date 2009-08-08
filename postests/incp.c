extern char* malloc(int);

void inc(int *x){
  *x = 100;
  return;
}

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);
  *r = 0;

  assert(*r == 0);
  inc(r);
  assert(*r == 100);
  
  return 0;
}
