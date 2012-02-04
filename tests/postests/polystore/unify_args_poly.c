#include <csolve.h>

void* LOC(L)
foo(void* LOC(L) x, void* LOC(L) y)
{
	x = y;
	return x;
}
