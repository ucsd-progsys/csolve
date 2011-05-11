extern int nondet ();

void one (int *x) {
    *x = 1;
}

void two (int *y) {
    *y = 2;
}

void invoke (void (*f) (int *)) {
    f ();
}

int main () {
    int (*f) ();

    f = nondet () ? &one : &two;
    invoke (f);

    return 0;
}
