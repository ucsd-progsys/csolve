void test () {
    return;
}

int main () {
    void (*f)();

    f = &test;
    f ();

    return 0;
}
