void main (int** y, int* z) {
    int i;

    *z = 10;
    *z = 20;

    i = 4;
    while (1) {
        i = i + 2;
        *(z + i) = i;
    }
    *(z + 8) = 50;

    *y = z;
    *(*y + 12) = 35;
}
