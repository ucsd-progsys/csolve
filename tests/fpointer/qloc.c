int inc (int *x) {
    return *x + 1;
}

int main () {
    int (*f) (int *);

    f = &inc;
    int z = f (0);
    assert (z >= 0);

    return 0;
}
