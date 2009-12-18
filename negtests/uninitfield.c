void f (int *y) {
    int x = *y;

    assert(0);
}

void main () {
    int z;

    f(&z);
}
