#include <cpj.h>

#define VALIDNODE START VALIDPTR ROOM_FOR(int)

void main(int
   * REF((4*(DEREF([V]):int)) = (VVADDR - BLOCK_BEGIN([VVADDR]))) VALIDNODE
   * ARRAY START VALIDPTR SIZE_GE(4*sz) l,
   int REF(V > 0) sz)
CHECK_TYPE
{
  foreach(i, 0, sz)
    int *n = l[i];
    int b = *n;
  endfor
}
