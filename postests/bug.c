/*int main(int a)
{
  //crashes
  for (; a; )
    0;
  return 0;
}

int main(int a)
{
  //does not crash
  if (a)
    0;
  return 0;
}

int main(int a)
{
  //does not crash
  int a;
  for (; a; )
    0;
  return 0;
}*/

/*int main(int a)
{
  //crashes
  while(a)
    0;
  return 0;
}*/

/*
int main()
{
  int b, c, d, e;
  b = 0;
  //c = 0; //works if this line uncommented
  
  //while (1)
  //while (b)
  for (; b < 2; b++) //also breaks
  {
    c = 0;
    assert (c == 0);
  }

  return 0;
}*/

int main()
{
  int b = nondet(), c;

/*  if (b != 0)
    b = 0;

  assert(b==0);
*/
  if (b != 0)
    b = 1; 
//  else
//    b = b;

  if (b == 0)
    c = 0;
  else
    c = 1;
  assert (b == c);
}
