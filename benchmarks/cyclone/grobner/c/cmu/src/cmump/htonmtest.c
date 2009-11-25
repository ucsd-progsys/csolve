#include <stdio.h>
#include <cmump.h>

main(void)
{
	MINT io;
	int b, blk;
	int cplen;
	char *cp;

	while (1) {
		MINIT(&io);
		printf("base: ");
		scanf("%d",&b);
		printf("blanks: ");
		scanf("%d",&blk);
		m_in_b(&io,b,stdin,blk);
		printf("\nOutbase: ");
		scanf("%d",&b);
		printf("Outblanks: ");
		scanf("%d",&blk);
		printf("\nOut: "); m_out_b(&io,b,stdout,blk);
		putchar('\n');
		cp = htonm(&io);
		cplen = msize(&io);
		MFREE(&io);
		printf("cplen = %d\n",cplen);
		MINIT(&io);
		ntohm(&io,cp);
		free(cp);
		printf("To-Network-To-Host: "); m_out_b(&io,b,stdout,blk);
		putchar('\n');
		if (feof(stdin)) break;
	}
}
