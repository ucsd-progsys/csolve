extern int* imalloc();

void main(){
  int *t;
  int x;

  t = imalloc();
  x = *t; 
  //OR DUALLY 
  *t = 10;
  
  return; 
}
