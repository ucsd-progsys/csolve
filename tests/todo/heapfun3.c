#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


typedef struct list {
  int id;
  struct list * VALIDPTR n;
};

int main(int argc, char ** argv)
{
  test(); 

  return 0;
}

struct list * INTLIST test()
{
  if (len <= 0)
    return NULL;

  struct list * l;
  struct list * root = l = malloc();
  root -> next = malloc();

  return root;
}




