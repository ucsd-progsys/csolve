extern int nondet ();

int *x;
char *c;

void one (int *a, char *b) {
    int *aa  = nondet () ? a : x;
    char *cc = nondet () ? c : b;
}

void two (int *t, char *u) {
    int *tt  = nondet () ? t : x;
    char *cc = nondet () ? c : u;
}

void main () {
    void (*f) (int *, char *);

    f = nondet () ? one : two;
}
