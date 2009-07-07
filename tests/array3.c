extern char* malloc(int);

void main(){
  int *x;
  int *y;
  int i;
  int z;

  y = (int *) malloc(100);
  
  x = y;
  i = 0;
  
  for (; i < 100; i++){  
    validptr(x);
    *x = 0;
    x++;
  }

  i = 0;
  x = y;
  for (; i < 100; i++){
    z = *x;
    assert(z >= 0);
    x++;
  }
  return 0;
}
