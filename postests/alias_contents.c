extern void *malloc (int);

int main () {
    int *x = (int *)malloc (sizeof (int));
    *x = nondet ();

    int *y = (int *)malloc (sizeof (int));
    *y = nondet ();

    if (nondet ()) {
        x = y;
    }

    // TODO: should work without this extra fold after we
    // finalize the concrete pointer y
    int z = *x;
    int w = *y;

    if (x == y) {
        assert (*x == *y);
    }
}
