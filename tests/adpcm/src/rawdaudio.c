/* testd - Test adpcm decoder */

#include "adpcm.h"
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#define NSAMPLES 1000

main() {
    int n;

    // pmr: inlined
    struct adpcm_state state;
    char	abuf[NSAMPLES/2];
    short	sbuf[NSAMPLES];

    while(1) {
	n = read(0, abuf, NSAMPLES/2);
	if ( n < 0 ) {
	    perror("input file");
	    exit(1);
	}
	if ( n == 0 ) break;
	adpcm_decoder(abuf, sbuf, n*2, &state);
	write(1, sbuf, n*4);
    }
    fprintf(stderr, "Final valprev=%d, index=%d\n",
	    state.valprev, state.index);
    exit(0);
}
