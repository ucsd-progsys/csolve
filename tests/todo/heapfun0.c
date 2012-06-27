#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


typedef struct list {
  int id;
  struct list * START NNVALIDPTR n;
};

int main(struct list * START NNVALIDPTR l)
{
  if (l == NULL)
    return 0;

  while (l != NULL)
    l = l->n;

  return 0;
}
