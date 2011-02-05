void main(){
  int i, j; 
 
  i = 0;
  j = 0;
  
  while (nondet()){
    i++;
    j++;
  }

  assert(i==j);
 
}
