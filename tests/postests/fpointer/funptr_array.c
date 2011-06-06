//! run with --manual

extern void *malloc (int);
extern int nondet ();

void one () { }

void two () { }

typedef void (*voidFunPtr) ();

int main () {
    voidFunPtr *fp = (voidFunPtr *) malloc (sizeof (voidFunPtr) * 2);

    fp[0] = &one;
    fp[1] = &two;

    int i = nondet ();
    if (0 <= i && i < 2) {
        fp[i] ();
    }
}
