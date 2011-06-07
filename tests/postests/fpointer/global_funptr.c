int (*f) ();

int one () {
    return 1;
}

int main () {
    f = &one;
    assert (f () > 0);

    return 0;
}
