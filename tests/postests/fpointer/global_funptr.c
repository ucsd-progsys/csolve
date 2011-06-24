int (*f) ();

int one () {
    return 1;
}

int main () {
    f = &one;
    int (*g)() = f;

    if (g != 0)
        assert (g () > 0);

    return 0;
}
