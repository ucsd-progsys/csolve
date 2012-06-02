#include <csolve.h>
#include <stdlib.h>

int* LOC(L) id(int* LOC(L) x)
{ 
  return x; 
}

void apply(void * LOC(L) p, void* LOC(L) (*fptr)(void* LOC(L) x))
{
  fptr(p);
}

void main() 
{
  int *y = malloc(2);
  int x;
  apply(&x, id);
}
