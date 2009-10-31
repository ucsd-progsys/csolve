extern char* malloc(int);

void main(){
  int *r;
  r = (int*) malloc(sizeof(*r));
  validptr(r);
  *r = 10;
  validptr(r);
  assert(*r == 10);
}
