extern void *malloc (int);
extern int nondet ();

void one (int *x) {
    *x = 1;
}

void two (int *y) {
    *y = 2;
}

typedef void (*voidFunPtr) ();

int main () {
    voidFunPtr *fp = (voidFunPtr *) malloc (sizeof (voidFunPtr) * 2);
    int i = nondet ();

    fp[0] = &one;
    fp[1] = &two;
    if (0 <= i && i < 2) {
        fp[i] = nondet () ? &one : &two;
    }

    fp[0] ();
    fp[1] ();
}
