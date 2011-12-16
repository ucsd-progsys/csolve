#include <stdlib.h>

int adpcm_coder()
{
  int len;
  int bufferstep;
  int n, nsamples;

  int *inp;
  int *outp;
  int *outp0;

  n = nondetpos(); 
  nsamples = 2*n;

  inp  = (int *)malloc(nsamples * sizeof(int));
  outp = (int *)malloc(n * sizeof(int));


  bufferstep = 1;
  
  len = nsamples;
  
  for(; 0 < len; len--)
  {
    //if ((inp - inp0) != (nsamples - len)){ csolve_assert(0); }
    //if (len <= 0){ csolve_assert(0); }
    //if (2 * (outp - outp0) != ((bufferstep - 1) + nsamples - len)) {csolve_assert(0);} 

    validptr(inp);
    *inp = 0;
    inp++;

    if (bufferstep == 0){
      validptr(outp);
      *outp = 0;
      outp++;
    } else {
      nondet();
    }
    
    bufferstep = 1 - bufferstep;
  }
  
  if (bufferstep == 0){
    validptr(outp);
  }
  
  return 0;
}

int adpcm_decoder()
{
  int len;
  int bufferstep;
  int n, nsamples;

  int *inp;
  int *outp;

  n = nondetpos(); 
  nsamples = 2*n;

  inp  = (int *)malloc(n * sizeof(int));
  outp = (int *)malloc(nsamples * sizeof(int));
  
  bufferstep = 0;
  
  len = nsamples;
  
  for(; 0 < len; len--)
  {
    //if ((inp - inp0) != (nsamples - len)){ csolve_assert(0); }
    //if (len <= 0){ csolve_assert(0); }
    //if (2 * (outp - outp0) != ((bufferstep - 1) + nsamples - len)) {csolve_assert(0);} 

    if (bufferstep == 0){
      validptr(inp);
      *inp = 0;
      inp++;
    } else {
      nondet();
    }
    bufferstep = 1 - bufferstep;
    
    validptr(outp);
    *outp = 0;
    outp++;
  }
  
  return 0;
}
