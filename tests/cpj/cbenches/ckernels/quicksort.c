

const int BUF_LEN;

int main(char ** argv, int argc)
{
  int * buf;
  int len = BUF_LEN;

  buf = malloc(sizeof(int) * BUF_LEN); 

  initialize(buf, len); 
  quicksort(buf, len);
}

//a: ptr(l, i) / l => (0+: int) -> len: int -> () / h: l => (0+; int)
//                                                  r: l => T
//                                                  w: l => T
void quicksort(int * a, int len)
{
  int end = len - 1;

  int mid = end / 2;

  if (*a > a[mid]) {
    //...
  }
  if (a[mid] > a[end]) {
    //...
  }
  if (*a > a[mid]) {
    //...
  }

  int lt = 1;
  int rt = end - 1;
  int pt = a[mid];

  while(1) {
    while(a[rt] > pt) rt--;
    while(lt < rt && a[lt] <= pt) lt++;

    if (lt < rt)
      swap(a[lt], a[rt--]);
    else break; 
  }

  cobegin {
    quicksort(a + lo, a + lt + 1);
    quicksort(a + lt + 1, a + hi - lt);
  }
}
