//testing contextual types for object uniqueness

#include <csolve.h>
#include <stdlib.h>

/* const int sz = 5; */
#define sz 5

typedef struct _obj obj;

struct _obj {
  //int x;
  int FINAL REF(V >= 0) id;
};

//int main(int argc, char ** argv)
//{
//  list * NNVALIDPTR * START ARRAY arr;
//  arr = malloc(sizeof(obj*) * sz);
//
//  foreach(i in 0 to sz)
//    arr[i] = alloc_obj(i);
//
//  foreach(i in 0 to sz)
//    arr[i] -> x = i;
//
//  foreach(i in 0 to sz)
//    arr[i] -> x = -i;
//}

obj * // REF(VV != 0 && VV.id = i)
alloc_obj(int REF(v >= 0) i)
{
  obj* o = malloc(sizeof(obj)); 
  o->id = i;
  //o->x = 1;
  return o;
}

void alloc_objs(obj * NNSTART NNVALIDPTR/* REF(VV != 0 => VV.id = vvind) */ * START ARRAY NONNULL arr)
{
//  foreach(i in 0 to sz)
//    arr[i] = alloc_obj(i);
  for (int i = 0; i < sz; i++){
    obj *o = alloc_obj(i);
    /* csolve_assert (o->id == i); */
    csolve_assert (o->id >= 0);
    arr[i] = o;
  }
}

obj * NNSTART NNVALIDPTR/* REF(VV.id = vvind) */ * START ARRAY NONNULL alloc_arr() 
{
  obj **arr = malloc(sizeof(obj*) * sz);
  alloc_objs(arr);
  return arr;
}

int main(int argc, char ** argv)
{
  obj ** arr = alloc_arr();
  csolve_assert (arr != NULL);
  for (int j = 0; j < sz; j++) {
      obj * a = arr[j];
      if (a != 0) {
        /* csolve_assert (arr[j]->id >= 0); */
	csolve_assert(a->id >= 0);
      }
      //csolve_assert (arr[j]->x);
  }

//  foreach(i in 0 to sz)
//    arr[i] -> x = i;

  //arr[i] :: {v: ptr(l2) | vv.id = i}
  //l2: (0: int

//  foreach(i in 0 to sz)
//    arr[i] -> x = -i;

  return 0;
}
