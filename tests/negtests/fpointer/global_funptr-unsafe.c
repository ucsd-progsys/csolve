int (*f) ();

int one () {
    return 0;
}

int main () {
    f = &one;
    assert (f () > 0);

    return 0;
}
