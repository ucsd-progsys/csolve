#include <csolve.h>

void * VAR(R)
foo(void * VAR(A) x, void * VAR(B) y)
{
	x = y;
	return x;
}
