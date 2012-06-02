#include <stdlib.h>
#include <csolve.h>

typedef void (* fptr)(void);

struct s {
  fptr f;
} * START VALIDPTR global_s;

extern void freer(struct s * VALIDPTR START sptr) OKEXTERN;

void hash_free(void)
{
  if(global_s)
    freer(global_s);
}


