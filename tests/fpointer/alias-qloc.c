extern int nondet ();

int inc (int *x) {
    return *x + 1;
}

int inc2 (int *y) {
    return *y + 2;
}

int main () {
    int (*f) (int *);

    f = nondet () ? &inc : &inc2;
    int z = f (0);
    assert (z >= 0);

    return 0;
}
