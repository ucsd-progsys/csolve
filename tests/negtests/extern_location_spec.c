extern void *malloc (int);

extern int *x;

void goog () {
    int *y = x;
    if (y != 0) assert (*y == 0);
}

void foof () {
    int *y = (int *) malloc (sizeof (int));
    *y = 0;
    x = y;

    goog ();
}
