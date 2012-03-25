//testing contextual types for object uniqueness
//note that listranking doesn't require this amount of generality, but this example is worth a try
#include <csolve.h>
#include <stdlib.h>

/* extern const int sz OKEXTERN; */

typedef struct _list {
  struct _list * NNVALIDPTR NNSTART next;
  int FINAL id;
  //  int x;
} list;

list* 
alloc_node(int i)
{
 list* o = malloc(sizeof(list)); 
 o->id = i;
 
 return o;
}

int 
main()//int argc, char ** argv) CHECK_TYPE
{
  int sz = 5;
  if (sz <= 0)
    return 0;

  list *root, *cur;
  int i = 1;

  root = alloc_node(0);
  for(cur = root; i < sz; i++)
  {
    cur->next = alloc_node(i);
    cur = cur->next;
  }
  
  for(cur = root; cur->next != NULL; cur = cur->next);
  {
    if(cur)
      validptr(cur->next);
  }
  /*   //cur :: { v: ptr(l1,.) | v.id = i} */
  /*   //l1 -> (4: {v: ptr(l2,.) | v = NULL || v.id = i + 1 ^ v != vvaddr - 4}) */
  
}
