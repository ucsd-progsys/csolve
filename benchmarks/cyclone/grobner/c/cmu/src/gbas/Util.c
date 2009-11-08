/* adapted from Parthenon, v5 */

#include <sys/time.h>
  


/* int elapsed_time(struct timeval *t1,struct timeval *t2) */
/* Compute the number of milliseconds between two times. */

FN int elapsed_time(t1,t2)

struct timeval *t1,*t2;

{
  long int sec,usec;
  
  sec=t2->tv_sec-t1->tv_sec;
  usec=t2->tv_usec-t1->tv_usec;
  if (usec < 0)
    {
      sec-=1;
      usec+=1000000;
    }
  return (1000*sec+usec/1000);
}




