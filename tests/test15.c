void main (int* z) {
    int i;
    int j;

    i = 0;
    while (1) {
        i = i + 2;
        z[i] = 20;
    }

    z[3] = 40;

    j = 1;
    z[j] = 30;
    while (1) {
        j = j + 2;
        z[j] = 30;
    }
}
