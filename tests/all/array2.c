extern char* malloc(int);

void main(){
  int *x;
  int i;

  x = (int *) malloc(100);
  
  i = 0;
  
  for (; i < 101; i++){  
    validptr(x);
    *x = 0;
    x++;
  }
  
  return 0;
}
