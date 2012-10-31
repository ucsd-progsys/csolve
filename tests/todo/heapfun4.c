#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


struct list {
  int id;
  struct list * START VALIDPTR n;
};

extern struct list * START INTLIST VALIDPTR magic() OKEXTERN;

int main()
{
  test(magic);

  return 0;
}

int test(struct list * START INTLIST VALIDPTR x)
{
  if(x == NULL)
    return 0;

  x -> n = malloc(sizeof(struct list));
}



