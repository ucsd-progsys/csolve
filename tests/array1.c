int main(){
  int n;
  int *x;
  int i;

  n = nondet();
  
  if (n < 0) { return 0;}

  x = malloc(n);
  
  i = 0 ;
  
  for(; i < n; i++){
    *x = 0;
    x++;
  }  

  return 0;

}
