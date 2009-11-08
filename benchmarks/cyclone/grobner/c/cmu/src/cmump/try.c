#include <cmump.h>
#include <stdio.h>

main(void)
{
	MINT p,q,gcd,rem;

	MINIT(&p); MINIT(&q); MINIT(&gcd);MINIT(&rem);
	min(&p);
	min(&q);
	mgcd(&p,&q,&gcd);
	mout(&gcd); printf("\n");
	mdiv(&p,&gcd,&p,&rem);
	mout(&p); printf("\n");
	mdiv(&q,&gcd,&q,&rem);
	mout(&q);
};

