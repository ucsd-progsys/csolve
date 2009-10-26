extern char *malloc(int);

void test (int * __attribute__ ((array)) p) {
    *p = 10;
}

void main () {
    int *q;

    q = (int *)malloc(sizeof(int) * 2);
    q[nondetpos()] = 0;
    test(q);
}
