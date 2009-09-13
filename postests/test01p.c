extern char* malloc(int);

int main(){
  int jhala = 1000;
  int *r;
  int y;

  r = (int*) malloc(10 * sizeof(int));
  
  //int r1[5] = { 1, 2, 3, 4};

  *r = 5;

  y = *r;

  assert(y >= 0);

  return 0;
}
