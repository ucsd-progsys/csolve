int maim () {
    int a = nondetpos ();
    int b = nondetpos ();

    while (a > 0 && b > 0) {
        a--; b--;
    }

    if (a > 0) {
        assert (b <= 0);
    }

    return 0;
}