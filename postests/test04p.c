extern char* malloc(int);

int main(){
  int *r;
  int z;

  r = (int*) malloc(4);

  *r = 1;
  assert(*r == 1);

  if (nondet()){ 
    z = 0;
  }

  *r = 2;
  assert(*r == 2);

  return 0;
}
