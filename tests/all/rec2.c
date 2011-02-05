
int facb(int z){
  int r;
  if (z <= 0) {
    r = 1;
  } else {
    r = faca(z-1);
    r = z * r;
  }
  return r;
}

int faca(int z){
  int r;
  if (z <= 0) {
    r = 1;
  } else {
    r = facb(z-1);
    r = z * r;
  }
  return r;
}

int main(int x){
  int y;
  y = faca(x);
  return y;
}

