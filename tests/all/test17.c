typedef struct test {
    int x;
    int arr[];
} test_t;

void main(test_t* t) {
    int i;

    t->x = 0;

    i = 0;
    while (i < 10) {
        i++;
        t->arr[i] = 10;
    }

    return;
}
