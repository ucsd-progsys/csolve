
void main1(int bound)
{
  int i, x;

  for(i = 0; i < bound; i++)
  {
    if (i & 1)
      x++;
    assert(2 * x < bound);
    assert(i < bound);
  }
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
