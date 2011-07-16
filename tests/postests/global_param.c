extern void *malloc (int);

int *x;

void foo (int **y) {
    *y = x;
}

void main () {
    int **z = malloc (sizeof (int *));

    foo (z);
}
