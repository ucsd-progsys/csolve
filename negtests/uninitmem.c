extern void *malloc(int);

void f (int *y) {
    int x = *y;

    assert(0);
}

void main () {
    int *z;

    z = (int *)malloc(sizeof(int));
    f(z);
}
