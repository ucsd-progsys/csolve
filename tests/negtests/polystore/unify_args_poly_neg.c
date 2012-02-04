#include <csolve.h>

void *
foo(void *x, void *y)
{
	x = y;
	return x;
}
