int y[1];

void foo (int x[]) {
  x[0] = 3;
}

int main () {
  foo (y);

  return 0;
}
