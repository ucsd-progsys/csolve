extern char* malloc(int);

int sum(int *buf, int *end){
  int sum = 0;
  while (buf <= end){
    validptr(buf);
    sum += *buf;
    buf++;
  }
  return sum;
}

int main(){
  int *x;
  int *z;
  int n,i,j,k;
  int res;
  int tmp;

  n = nondetpos();
  z = (int *) malloc(n);

  x = z;
  int i = 0;
  
  for (; i < n; i++){  
    validptr(x);
    tmp = nondet();
    *x  = tmp;
    x++;
  }

  int j = nondetpos();
  int k = nondetpos();
  if (j < k && k < n){
    validptr(z+j);
    validptr(z+k);
    res = sum(z+j, z+k);
  }
  return 0;
}
