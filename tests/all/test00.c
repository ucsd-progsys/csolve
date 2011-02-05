void main(){
  int x,y;

  x = nondet();

  if (x > 0){
    y = x;
  } else {
    y = 0-x;
  }

//  assert(y >= 0);

  return 0;
}
