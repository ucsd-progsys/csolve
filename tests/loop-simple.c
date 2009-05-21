void main(){
  int i, n; 
  n = nondet();
  for (i=0; i < n; i++){
    nondet();
  }
  assert (i >= 0);
}
