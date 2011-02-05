extern char* malloc(int);

/* TODO:
 * bufferstep toggle
 */

int main()
{
  int len;
  int bufferstep;
  int n, nsamples;

  /* 
  int *inp;
  int *outp;
  int *inp0;
  int *outp0;
   */
  
  int inp;
  int outp;
  int inp0;
  int outp0;
  
  n = nondetpos(); 
  nsamples = 2*n;

  inp0  = 0;//(int *)malloc(2*n);
  outp0 = 0;//(int *)malloc(n);
  
  inp  = inp0;
  outp = outp0;

  bufferstep = 1;
  
  len = nsamples;
  
  for(; 0 < len; len--)
  {
    if ((inp - inp0) != nsamples - len){ assert(0); }
    // assert(0); SANITY 
    inp++;
    if (len <= 0){ assert(0); }
    if (2 * (outp - outp0) != (bufferstep-1) + nsamples - len) {assert(0);} 
    //if (2 * (outp - outp0) > nsamples + 1){ assert(0); }

    if (bufferstep == 0){
      //access *outp
      outp++;
    } else {
    }
    bufferstep = 1 - bufferstep;
  }
  return 0;
}
