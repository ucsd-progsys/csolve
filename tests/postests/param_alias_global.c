extern int nondet ();

int *x;

void foo (int *y) {
    int *z = nondet () ? x : y;
}

void main () {
    int *w = 0;

    foo (w);
}
