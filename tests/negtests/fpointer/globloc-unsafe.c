extern void *malloc (int);

int a[] = { 0, 1, 2 };

void decGlobal () {
    a[0]--;
}

int main () {
    void (*f) ();
    int *x;

    f = &decGlobal;
    f ();
    lcc_assert (*a >= 0);

    return 0;
}
