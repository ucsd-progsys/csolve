#include "multpol.h"

short nvars=6;
char *varnames[]={"x","y","z","u","v","w"};
int first_group=3,order_exp;

main(){
  MPOL p,q;
  MINT coef;


  scanf("%d",&order_exp);
  init_order_exp();
  MPOLINIT(&p);
  MPOLINIT(&q);
  mpolin(&p);
  mpolin(&q);
  mpolsub(&p,&q,&q);
  mpolout(&q);
  printf("\n");
}; 

