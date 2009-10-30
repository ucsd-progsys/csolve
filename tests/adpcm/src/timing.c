/*
** Timing - Test timing on adpcm coder and decoder.
**
** The program creates 10Kb garbage, and runs the compressor and
** the decompressor on it.
*/

#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include <stdlib.h>
#include "adpcm.h"

#define DATASIZE 10*1024	/* Data block size */

#define ITERATIONS 2000

short pcmdata[DATASIZE];
char adpcmdata[DATASIZE/2];
short pcmdata_2[DATASIZE];

struct adpcm_state coder_1_state, coder_2_state, decoder_state;

main() {
    int i;
    int t0, t1, t2, t3;
    int count = 0, count2;

    for(i=0; i<DATASIZE; i++)
      pcmdata[i] = random() & 0xffff;

    for(i=0;i<ITERATIONS;i++) {
      adpcm_coder(pcmdata, adpcmdata, DATASIZE, &coder_1_state);
    }

    for(i=0;i<ITERATIONS;i++) {
      adpcm_coder(pcmdata, adpcmdata, DATASIZE, &coder_2_state);
      adpcm_decoder(adpcmdata, pcmdata_2, DATASIZE, &decoder_state);
    }
    exit(0);
}
