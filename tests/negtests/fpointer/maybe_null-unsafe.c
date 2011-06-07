extern int nondet ();

void doNothin () { }

int main () {
    void (*f) ();

    f = nondet () ? 0 : &doNothin;
    f ();

    return 0;
}
