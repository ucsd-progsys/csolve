void main(){
  int i, p, q; 
  int n;

  n = nondet();
  q = nondet();

  p = q;
  i = 0;
  
  for(; i < n; i++){
    p++;
  }
 
  assert(p == q+i);

  return;
}
