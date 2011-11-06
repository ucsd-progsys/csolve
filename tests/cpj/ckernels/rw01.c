#include <cpj.h>
#include <stdlib.h>


typedef struct
{
  int x;
  int y;
} t;

int main(void)
{
  t* z = malloc(sizeof(t)); 
  z->x = 0;
  z->y = 0;
  
  cobegin
    read(z);
    write(z);
  coend

  return 0;
}

void read(t* z)
{
  z->x; 
}

void write(t* z)
{
  z->y = 1;
}
