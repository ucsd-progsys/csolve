extern char* malloc(int);

void inca(int *x){
  //if (nondet()) incb(x);
  *x = *x + 1;
  return;
}

void incb(int *x){
  //if (nondet()) inca(x);
  *x = *x + 1;
  return;
}

/*
void incc(int *x){
  *x = *x + 1;
  return;
} 
*/

void main(){
  int *r;

  r = (int*) malloc(4);
  *r = 0;
  
  inca(r);
  incb(r);
 // incc(r);

  assert(*r >= 0);
  
  return;
}
