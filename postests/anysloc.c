extern char *malloc(int);

void test (int *p) {
    *p = 10;
}

void main () {
    int *q;

    q = (int *)malloc(sizeof(int) * 2);
    q[0] = 1;
    q[1] = 2;

    test(q);
}
