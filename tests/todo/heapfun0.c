typedef struct list {
  struct list * n;
  int id;
}; 

struct list * / unq(L, 0:int) NNVALIDPTR mk(int len) / emp
{
  if (len <= 0)
    return NULL;

  struct list * l;
  struct list * root = l = malloc();
  root -> next = malloc();

  return root;
}




