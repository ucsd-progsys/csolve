void t (int *x) {
    /* assert (0); */
    *x = 3;
    assert (0);
}

int main () {
    int x = 0;

    t (&x);

    return 0;
}
