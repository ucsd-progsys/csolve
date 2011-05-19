void foop (int *x, char *c) {
}

int main () {
    int *p;

    foop (p, (char *) p);

    return 0;
}
