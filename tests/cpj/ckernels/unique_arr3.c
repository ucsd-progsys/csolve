//testing contextual types for object uniqueness

#include <csolve.h>
#include <stdlib.h>

#define OBJLINK ((DEREF([V]) : int) = (VVADDR - BLOCK_BEGIN([VVADDR])))

typedef struct _obj {
  int FINAL id;
} obj;

void
alloc_objs(obj *
               * ARRAY VALIDPTR START SIZE(4*sz) arr,
	   int  REF(V >= 0)                       sz)
/* /\* CHECK_TYPE *\/ */
{
  int x = 0;
  for(int i = 0; i < sz; i++)
  {
    if (arr[i] != NULL) {
      csolve_assert(arr[i]->id >= 3);
      /* csolve_assert(arr[i]->id == 4*i); */
    }
  }
}

/* extern void do_thing(obj * START * ARRAY START VALIDPTR arr) OKEXTERN; */

void main(int REF(V >= 0) sz)
{
  /* int sz = 5; */
  obj **arr = malloc(sizeof(*arr)*sz);
  
  for (int i = 0; i < sz; i++) {
    obj *x = malloc(sizeof(*x));
    x->id = 3;
    //    x->id = 4*i;
    arr[i] = x;
  }
  
  
  /* validptr(arr); */
  /* do_thing(arr); */
  
   alloc_objs(arr, sz);
}
