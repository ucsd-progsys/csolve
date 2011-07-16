extern int nondet ();

int *x;

void foo (int *y) {
    int *z = nondet () ? x : y;
}

void main () {
    void (*f) (int *);

    f = foo;
}
