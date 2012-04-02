#include <cpj.h>

#define VALIDNODE START VALIDPTR ROOM_FOR(struct node)

struct node {
  int FINAL id;
  int foo;
};

void main(struct node
   * REF((4*(DEREF([V]):int)) = (VVADDR - BLOCK_BEGIN([VVADDR]))) VALIDNODE FINAL
   * ARRAY START VALIDPTR SIZE_GE(4*sz) l,
   int REF(V > 0) sz)
CHECK_TYPE
{
  foreach(i, 0, sz)
    struct node *n = l[i];
    int b = n->id;
    csolve_assert(b == i);
    n->foo = 3;
  endfor
}
