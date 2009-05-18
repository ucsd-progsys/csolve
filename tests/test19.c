typedef struct test {
    int x;
    int y;
} test_t;

void main(test_t* t) {
    int* yp;
    int* xp;

    yp = &t->y;
    xp = yp - 1;

    *xp = 100;
    *yp = 200;

    return;
}
