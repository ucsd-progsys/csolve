
void main(){
  int i, j, b; 
 
  j = 0; 
  
  //b = nondet(); j = b;
  
  for(i = 0; i < 10; i++)
    j++;

  assert(0 <= j);
  assert(j == 10);
 
  //assert(b <= j && j <= b + 10);
}
