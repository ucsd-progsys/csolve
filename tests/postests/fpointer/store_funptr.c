extern void *malloc (int);

void one () { }

void two () { }

typedef void (*voidFun) ();

int main () {
    voidFun *fp;
    voidFun f;

    fp = (voidFun *) malloc (sizeof (voidFun));
    *fp = nondet () ? &one : &two;
    f   = *fp;
    f ();

    return 0;
}
