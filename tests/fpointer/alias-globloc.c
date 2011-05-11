extern int nondet ();
extern void *malloc (int);

int *domination = 0;

void incGlobal () {
    *domination = 0;
}

void incGlobal2 () {
    *domination = 1;
}

int main () {
    void (*f) ();

    f = nondet () ? &incGlobal : &incGlobal2;

    domination = (int *) malloc (sizeof (int));
    f ();

    return 0;
}
