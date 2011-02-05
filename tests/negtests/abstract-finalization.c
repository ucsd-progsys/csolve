extern void *malloc (int);

int main () {
    int *x = (int *) malloc (sizeof (int));
    int *y = (int *) malloc (sizeof (int));

    *x = 0;
    *y = 0;

    if (nondet ()) {
        x = y;
    }

    if (*x == *y) {
        assert (*x == *y);
    }

    *x = 1;

    return 0;
}
