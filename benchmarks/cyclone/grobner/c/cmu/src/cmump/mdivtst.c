#include <cmump.h>

main(void)
{
	MINT a,b,c,d;
	MINIT(&a);MINIT(&b);MINIT(&c);MINIT(&d);
	printf("a = "); min(&a);
	printf("b = "); min(&b);
	mdiv(&a,&b,&c,&d);
	printf("c = "); mout(&c); putchar('\n');
	printf("d = "); mout(&d); putchar('\n');
}
