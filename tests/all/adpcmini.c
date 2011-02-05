int main1()
{
  int i, x;
  int b;
  int bound;

  bound = nondet();

  x = 0;
  b = 0;
  i = bound;
  
  for(; 0 < i; i--)
  {
    if (b==0){
      b = 1;
      x++;
    } else {
      b = 0;
    }
    if (i <= 0){ assert(0); }
    if (2*x != b + bound - (i - 1)) {assert(0);} 
    if (2*x > bound + 1){ assert(0); }
  }
  
  return 0;
}

/*
int main2()
{
  int i, x, bound, twox;
  int b;

  bound = nondet();
  x = 0;
  b = 0;
  i = 0;
  
  for(; i < bound; i++)
  {
    if (b==0){
      b = 1;
      x++;
    } else {
      b = 0;
    }
    if (i >= bound){ assert(0); }
    if (2*x != b + (i + 1)) {assert(0);} 
    if (2*x > bound + 1){ assert(0); }
  }
  
  return 0;
}
*/

/*
void main3(int bound)
{
  int i, x, flag = 1;

  int sz = sizeof(int) * bound;
  int a = malloc(sz);
  int b = malloc(sz / 2);

  for(i = 0; i < bound; i++)
  {
    a[i] = 0;
    b[x] = 0;

    if (flag)
      x++;
    flag = !flag; 
    //assert(2 * x < bound);
    //assert(i < bound);
    //assert(2 * x = i + flag);
  }
}*/
