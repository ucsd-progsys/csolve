int g = 20;
int a[] = {0, 1, 2};

void f() {
    g = 0;
}

void h() {
    f();
    g = 1;
}
