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
void adpcm_coder ARGS((short * COUNT(nsample) NONNULL,
                       char * COUNT(nsample / 2)  NONNULL, int nsample,
                       struct adpcm_state *));
void adpcm_decoder ARGS((char * COUNT(nsample / 2)  NONNULL,
                         short * COUNT(nsample)  NONNULL, int nsample,
                         struct adpcm_state *));
