#include <csolve.h>
#include <stdlib.h>

extern
char * REF(TAG([V]) = TAG([fd])) SIZE_GE(1)
read(int fd)
  OKEXTERN;

extern
write(int REF(TAG([V]) = TAG([d])) fd, char * SIZE_GE(1) d)
  OKEXTERN;


extern
same_policy(int reader,
            void * REF(((TAG([V]) = reader) ==> TAG([V])) <==> (TAG([V]) = reader) 
                       , void *d2);

/* is_reader(v, a) == 1 */

char * SIZE_GE(1)
  main(int REF(|| [TAG([V]) = V; TAG([V]) = fd2]) REF(V > 0) fd1,
       int REF(|| [TAG([V]) = V; TAG([V]) = fd1]) REF(V > 0) fd2)
  CHECK_TYPE
{
  char *x = read(fd1);
  char *y = read(fd2);
  write(fd1, x);
  write(fd2, x);
  //  write(fd2, y);
  return x;
}

/* write(TAG(V,V) fd, TAG(V, fd) buffer) */
/* Two things happening
   - Subtyping (function arguments)
   - Information flow policy (spec of read/write)
   Consider these things separately
*/
   
   
