extern int nondet ();
extern void *malloc (int);

int  *g1 = 0;
char *g2 = 0;

void foo () {
    *g1 = 5;
}

void bar () {
    *g2 = 'a';
}

int main () {
    void (*f) ();

    f = nondet () ? &foo : &bar;
    f ();

    return 0;
}
