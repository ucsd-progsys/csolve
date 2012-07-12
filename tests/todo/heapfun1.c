#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


typedef struct list {
  int id;
  struct list * START NNVALIDPTR n;
};

int main(struct list * START INTLIST LOC(L) NNVALIDPTR l1,
         struct list * START INTLIST LOC(L) NNVALIDPTR l2)
{
  if (l1 == NULL || l2 == NULL)
    return 0;

  while (l1 && l2)
  {
    l1 = l1 -> n;
    l2 = l2 -> n;
  }

  return 0;
}




