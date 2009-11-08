#include "multpol.h"

short nvars=5;

main(){
  short p1[10],p2[10],p3[10];
  int i;

  for (i=0;i<5;i++) scanf("%hd",&p1[i]);
  for (i=0;i<5;i++) scanf("%hd",&p2[i]);
  expofactor(p1,p2,p3);
  for (i=0;i<5;i++) printf("%d ",(int)p3[i]);
  
}; 

