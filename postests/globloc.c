int p[] = { 0, 1, 2 };

void f() {
    p[0] = 1000;
}

void main () {
    int x;

    x = *p;

    f();

    assert (x <= 1000);
}
