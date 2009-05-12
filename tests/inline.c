int f(int x, int y) {
    return x * y;
}

int g(int x) {
    return f(x, x + 1);
}

void main() {
    int x;

    x = 0;
    x = 1;
    x = 2;
    x = g(g(x));
    x = 1;
    x = 2;

    return;
}
