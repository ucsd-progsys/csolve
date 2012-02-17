#include <csolve.h>

void* LOC(L) id(void * LOC(L) x)
{ 
  return x; 
}

void apply(void *p, void * LOC(L) (*fptr)(void * LOC(L) x))  CHECK_TYPE
{
  fptr(p);
}

void main() 
{
  int x;
  apply(&x, id);
}
