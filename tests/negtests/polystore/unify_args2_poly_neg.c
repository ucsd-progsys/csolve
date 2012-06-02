#include <csolve.h>

int 
foo(void *VAR(a) *x, void *VAR(b) *y) CHECK_TYPE
{
	*x = *y;
	return 0;
}
