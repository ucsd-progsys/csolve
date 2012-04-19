typedef struct list {
  struct list * n;
  int id;
}; 

//no refinements yet!
int REF(V >= 0) / unq(L, 0:int) len(struct list LOC(L) * l) / unq(L, 0:int)
{
  //for(int i = 0; l != NULL; i++, l = l -> n); 
  int i = 0;
  while(l != NULL)
  {
    //unfold [l_j -> 0:int; 4: ptr(L') * L', l_j <: L / unq(L, 0:int)]
    l = l -> n;  
    i++;
    //fold   [unq(L, 0:int) / l_j -> 0: int; 4: ptr(L') * L', l_j <: L]
  }
  return i; 
}

struct list * / unq(L, 0:int) NNVALIDPTR mk(int len) / emp
{
  if (len <= 0)
    return NULL;

  struct list * l;
  struct list * root = l = malloc();

  for(int i = 1; i < len; i++, l = l -> n)
    l -> n = malloc(struct list);

  return root;
}




