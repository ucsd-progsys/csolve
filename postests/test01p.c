extern char* malloc(int);

int main(){
  int jhala = 1000;
  int *r;
  int y;

  r = (int*) malloc(4);

  *r = 5;

  y = *r;

  assert(y >= 0);

  return 0;
}
