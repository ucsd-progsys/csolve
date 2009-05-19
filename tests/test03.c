void assert(int b){
  return;
}

int abs(int x){
  if (x > 0){
    return x;
  } else {
    return (0-x);
  }
}

void main(int x){
  int y;

  y = abs(x);
  assert(y >= 0);
  return;
}
