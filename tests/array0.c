extern char* malloc(int);

int y[10];

void foo(int x[]/*__attribute__((array))*/){
  return;
}


void main(){
  foo(y);
  return 0;
}
