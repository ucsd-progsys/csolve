#include <stdio.h>

main (int argc, char **argv)
{
	extern float itimerread(int timerid);
	extern void itimerinit(int timerid, int setval);
	int loops, i;
	float before, after;

	sscanf ( argv[1], "%d", &loops );
	itimerinit ( 1, 1000 );
	before = itimerread ( 1 );
	for ( i = 0; i < loops; i++ )
		;
	after = itimerread ( 1 );
	printf ( "%f\n", before - after );
}

