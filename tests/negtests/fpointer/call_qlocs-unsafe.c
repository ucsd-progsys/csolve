extern int nondet ();
extern void *malloc (int);

typedef void (*setter) (int *);

void one (int *x) {
    *x = 1;
}

void two (int *y) {
    *y = -1;
}

void invoke (setter f) {
    int *z = (int *) malloc (sizeof (int));

    f (z);
    assert (*z >= 0);
}

void main () {
    setter f;

    f = nondet () ? &one : &two;
    invoke (f);

    return;
}
