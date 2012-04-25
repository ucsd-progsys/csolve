#include <csolve.h>

int 
foo(void **x, void **y) CHECK_TYPE
{
	*x = *y;
	return 0;
}
