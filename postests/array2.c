extern char* malloc(int);

void main(){
  int *x;
  int *y;

  int i;

  y = (int *) malloc(100);
  
  x = y;

  i = 0;
  
  for (; i < 100; i++){  
    validptr(x);
    *x = 0;
    x++;
  }
 

  return 0;
}
