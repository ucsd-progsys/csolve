
int fac(int z){
  int r;
  if (z <= 0) {
    r = 1;
  } else {
    r = fac(z-1);
    r = z * r;
  }
  return r;
}

int main(int x){
  int y;
  y = fac(x);
  return y;
}

