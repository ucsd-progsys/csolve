//testing contextual types for object uniqueness
//note that listranking doesn't require this amount of generality, but this example is worth a try
#include <csolve.h>
#include <stdlib.h>

typedef struct _list {
  int FINAL id;
  struct _list * FINAL n;
} list;

struct _list;
typedef struct _list list;


list * alloc_node(int i)
{
  list * o = malloc(sizeof(list)); 
  o->id    = i;
  return o;
}

void check_list(list * l, int sz) 
{
  list * c = l;
  int i = 0;
  for(i = 0, c = l; c != NULL, i < sz; c = c->n)
    csolve_assert(c->id == i);
}

int main(int argc, char ** argv)
{
  int sz = nondetpos() + 1;

  list * root;
  list * cur;
  int i;

  root = alloc_node(0);
  for(i = 1, cur = root; i < sz; i++, cur = cur->n)
    cur->n = alloc_node(i); 
    //cur :: { v: ptr(l1,.) | v.id = i}
    //l1 -> (4: {v: ptr(l2,.) | v = NULL || v.id = i + 1 ^ v != vvaddr - 4})
  cur->n = NULL;

  check_list(root, sz);

  return 0;
}
