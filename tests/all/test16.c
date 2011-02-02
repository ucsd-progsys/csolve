typedef struct test {
    int x;
    int y;
} test_t;

void main (test_t* t) {
    t->x = 3;

    int i = 0;
    while (i < 10) {
        i = i + 1;
        t[i].x = 5;
        t[i].y = 10;
    }

    return;
}
