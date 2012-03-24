#include <cpj.h>
//#include <stdlib.h>

#define VALIDNODE START VALIDPTR ROOM_FOR(node)
#define NNVALIDNODE NNSTART NNVALIDPTR NNROOM_FOR(node)

/* typedef struct node */
/* {  */
/*   int FINAL id; */
/*   /\* struct node * NNVALIDNODE next;  *\/ */
/*   /\* struct node * NNVALIDNODE rankNbr; *\/ */
/*   /\* int rank; *\/ */
/*   /\* int savedRank; *\/ */
/*   /\* struct node * NNVALIDNODE savedRankNext; *\/ */
/* } node; */

typedef int node;

//void foo(int *n) { int b = *n; }
void rank(node 
   * REF((4*(DEREF([V]):int)) = ((VVADDR - BLOCK_BEGIN([VVADDR])):int))  VALIDNODE 
   * ARRAY START VALIDPTR SIZE_GE(4*sz) l, int REF(V > 0) sz) CHECK_TYPE
{
  foreach(i, 0, sz)
    node *n = l[i];
    int b = *n;
    /* csolve_assert(*n == i); */
  endfor
}
