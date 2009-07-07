extern char* malloc(int);

int main(){
  int *r;
  int y;

  r = (int*) malloc(4);

  *r = 5;

  y = *r;

  assert(y >= 10);

  return 0;
}
