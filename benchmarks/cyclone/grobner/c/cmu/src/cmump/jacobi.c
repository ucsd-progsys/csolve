#include <cmump.h>

main(void)
{
	MINT p,q;
	MINIT(&p); MINIT(&q);
	for (;;) {
		printf("p = ");
		min(&p);
		if (!mtest(&p)) {
			printf("\nBye\n");
			exit(0);
		}
		printf("q = ");
		min(&q);
		if (!mtest(&q)) {
			printf("\nSee ya!\n");
			exit(0);
		}
		printf("J(");mout(&p);printf(",");mout(&q);printf(") = %d\n",mjacobi(&p,&q));
	}
}
