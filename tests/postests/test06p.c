extern void *malloc (int);

void main(){
  int * __attribute__ ((unchecked)) t;
  int x;

  t = malloc(0);
  x = *t; 
  //OR DUALLY 
  *t = 10;
  
  return; 
}
