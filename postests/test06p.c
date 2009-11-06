//! run with -manual

extern int* imalloc();

void main(){
  int * __attribute__ ((unchecked)) t;
  int x;

  t = imalloc();
  x = *t; 
  //OR DUALLY 
  *t = 10;
  
  return; 
}
