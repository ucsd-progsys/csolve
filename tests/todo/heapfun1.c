#include <csolve.h>
#include <cpj.h>
#include <stdlib.h>


typedef struct list {
  int id;
  struct list * START NNVALIDPTR n;
};

extern struct list * INTLIST START NNVALIDPTR magic() OKEXTERN;

int main(int argc, char ** argv)
{
  test(magic()); 

  return 0;
}

void test(struct list * START INTLIST NNVALIDPTR l)
{
  if (l == NULL)
    return;

  while (l != NULL)
    l = l -> n;
}




