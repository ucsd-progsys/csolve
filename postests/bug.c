int main(int a)
{
  (* crashes *)
  for (; a; )
    0;
  return 0;
}

int main(int a)
{
  (* does not crash *)
  if (a)
    0;
  return 0;
}

int main(int a)
{
  (* does not crash *)
  int a;
  for (; a; )
    0;
  return 0;
}
