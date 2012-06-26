#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


typedef struct list {
  int id;
  struct list * NNVALIDPTR n;
};

extern struct list * INTLIST NNVALIDPTR magic() OKEXTERN;

int main(int argc, char ** argv)
{
  test(magic()); 

  return 0;
}

void test(struct list * NNVALIDPTR l)
{
  struct list * ll = l;

  if (ll == NULL)
    return;

  while (ll != NULL)
    ll -> n;
    //ll = ll->n;
}




