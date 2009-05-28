extern char* malloc(int);

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);

  *r = 0;

  while (nondet()){
    *r++;
  }

  assert(*r >= 0);

  return 0;
}
