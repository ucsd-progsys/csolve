void test(int* i) {
    return;
}

void main() {
    int i;

    i = 0;
    test(&i);

    return;
}
