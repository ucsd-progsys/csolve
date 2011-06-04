extern int nondet ();
extern void *malloc (int size);

void test () {
    return;
}

int main () {
    void (*f) ();
    int *ip = (int *) malloc (sizeof (int));

    *ip = 3;

    if (nondet ()) {
        f = &test;
    } else {
        f = ip;
    }

    return 0;
}
