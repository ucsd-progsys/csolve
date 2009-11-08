#include <cmump.h>
#include <stdio.h>

main(void)
{
	MINT a,b,q,r;

	MINIT(&a); MINIT(&b); MINIT(&q); MINIT(&r);
	while ((fputs("a: ",stdout), min(&a) != EOF) && (fputs("b: ",stdout), min(&b) != EOF)) {
		mdiv(&a,&b,&q,&r);
		mout(&a); putchar('\n');
		mout(&b); putchar('\n');
		mout(&q); putchar('\n');
		mout(&r); putchar('\n');
		mmod(&a,&b,&r);
		mout(&a); putchar('\n');
		mout(&b); putchar('\n');
		mout(&r); putchar('\n');
	}
	exit(0);
}
