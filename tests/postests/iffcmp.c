int main () {
    int cmp;
    int a = nondet ();
    int b = nondet ();

    cmp = a > b;

    while (nondet ()) {
        a = nondet ();
        b = nondet ();
        cmp = a > b;
    }

    if (cmp) {
        assert (a > b);
    }
}