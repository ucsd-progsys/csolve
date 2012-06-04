#include <csolve.h>

int 
foo(void *VAR(a) *x, void *VAR(a) *y) CHECK_TYPE
{
	*x = *y;
	return 0;
}
