#include <csolve.h>

int 
foo(void * LOC(L) *x, void * LOC(L) *y) CHECK_TYPE
{
	*x = *y;
	return 0;
}
