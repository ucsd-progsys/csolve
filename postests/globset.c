int x = 5;

void f() {
    assert(x >= 0);
}

void main () {
    x = 10;
    f();
}
