void test () {
    return;
}

int main () {
    void (*f)();

    f = &test;

    return 0;
}
