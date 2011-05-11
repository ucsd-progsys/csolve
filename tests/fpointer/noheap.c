int test () {
    return 15;
}

int main () {
    int (*f)();

    f = &test;
    f ();

    return 0;
}
