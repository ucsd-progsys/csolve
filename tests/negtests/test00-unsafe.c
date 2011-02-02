void main(){
  int i;

  i = 0;
  while (nondet()){
    i++;
  }

  assert (i < 0);
}
