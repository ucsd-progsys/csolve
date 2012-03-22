//testing contextual types for object uniqueness
#include <csolve.h>
#include <stdlib.h>

extern const int sz;

typedef struct _list list;

struct _list {
  int x;
  list * n;
  int FINAL id;
}

list* alloc_node(int i)
{
 list* o = malloc(sizeof(list)); 
 o.id = i;
}

int main(int argc, char ** argv)
{
  if (sz <= 0)
    return 0;

  list * root, cur;
  int i;

  root = alloc_node(0);
  for(i = 1, cur = root; i < sz; i--, cur = cur -> next)
    cur->next = alloc_node(i); 
    //cur :: { v: ptr(l1,.) | v.id = i}
    //l1 -> (4: {v: ptr(l2,.) | v = NULL || v.id = i + 1 ^ v != vvaddr - 4})
  cur -> next = null;
}
