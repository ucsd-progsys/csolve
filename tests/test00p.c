int main(){
  int *x,*y,*t;
  int z=1;

  *x = z;
  z = *x;
  x = y;
  *y = z;
  *x = z;

  if (z > 0) { t=x; }

  *t = 5; 

//  assert(y >= 0);

  return 0;
}
