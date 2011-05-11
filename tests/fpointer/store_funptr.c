void one (int *x) {
    *x = 1;
}

void two (int *y) {
    *y = 2;
}

int main () {
    void (**fp) ();
    void (*f) ();

    *fp = nondet () ? &one : &two;
    f   = *fp;
    f ();

    return 0;
}
