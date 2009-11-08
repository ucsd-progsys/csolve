#include <malloc.h>

char *xalloc (int nb)
{
	return (char*) galloc ( nb );
}

void xfree (char *ap)
{
	gfree ( ap );
	return;
}

