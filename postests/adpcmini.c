
int main1()
{
  int i, x, bound;
  int t1, t2;
  int b, c;

  i = x = bound = b = 0;
  
  do
    bound = nondet();
  while(bound < 0);

  assert (bound >= 0);

  i = 0;
  for(; i < bound; i++)
  {
    t1 = i / 2;
    t2 = 2 * t1;
    if (t2 == i)
      b = 1;
    else
      b = 0;
    x += b;
    assert(2 * x < bound);
    //assert(i < bound);
  }
  return 0;
}

/*void main2(int bound)
{
  int i, x, flag = 1;

  for(i = 0; i < bound; i++)
  {
    if (flag)
      x++;
    flag = !flag; 
    assert(2 * x < bound);
    assert(i < bound);
    //assert(2 * x = i + flag);
  }
}

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
