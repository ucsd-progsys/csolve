int main(int n){
  int i, z; 
  //n = nondet();
  
  assert(10 < n);

  z = 0;
  for (i=0; i < n; i++){
    z += i;
    z += i;
    z += i;
    z += i;
  }
  assert (z >= 0);
  return 0;
}
