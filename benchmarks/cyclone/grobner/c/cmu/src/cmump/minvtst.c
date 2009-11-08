#include <stdio.h>
#include <cmump.h>
main(void)
{
	MINT a,b,c;
	MINIT(&a);MINIT(&b);MINIT(&c);
	while ((fputs("a: ",stdout),min(&a) != EOF) &&
	       (fputs("b: ",stdout),min(&b) != EOF)) {
		minvert(&a,&b,&c);
		fputs("c = ",stdout); mout(&c); putchar('\n');
	}
	exit(0);
}
