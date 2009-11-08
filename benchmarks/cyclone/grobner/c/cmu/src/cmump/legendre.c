#include "cmump.h"

main(int ac, char **av)
{
	MINT p,q;
	int count = 1, jacobi, i;

	if (ac > 1) count = atoi(av[1]);
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
		for (i = 0; i < count; i++) jacobi = mlegendre(&p,&q);
		printf("J(");
		mout(&p);
		printf(",");
		mout(&q);
		printf(") = %d\n",jacobi);
	}
}
