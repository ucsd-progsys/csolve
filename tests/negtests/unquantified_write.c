//! run with --manual

extern int nondet ();
extern void *malloc (int);

int *x;

void foo (int *y) {
    int *z = nondet () ? x : y;
    // pmr: Are globals getting initialized correctly in the scalar stuff?
    /* *z = -1; */
}

void main () {
    x = (int *) malloc (sizeof (int));
    *x = 12;

    int *a = (int *) malloc (sizeof (int));
    *a = -12;

    foo (a);

    assert (*x > 0);
}
