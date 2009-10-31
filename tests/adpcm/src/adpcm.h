/*
** adpcm.h - include file for adpcm coder.
**
** Version 1.0, 7-Jul-92.
*/

struct adpcm_state {
    short	valprev;	/* Previous output value */
    char	index;		/* Index into stepsize table */
};

#ifdef __STDC__
#define ARGS(x) x
#else
#define ARGS(x) ()
#endif

#ifndef DEPUTY
  #define COUNT(x)
  #define NONNULL
#endif

#define ARRAY __attribute__ ((array))

void adpcm_coder ARGS((short * ARRAY,
                       char * ARRAY, int nsample,
                       struct adpcm_state *));
void adpcm_decoder ARGS((char * ARRAY,
                         short * ARRAY, int nsample,
                         struct adpcm_state *));
