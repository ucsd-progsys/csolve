#include <stdio.h>
#include <cmump.h>

main(void)
{
	MINT io;
	int b, blk;

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
		if (feof(stdin)) break;
	}
}
