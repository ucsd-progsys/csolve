extern int nondet ();

void foo () {
    return;
}

void bar () {
    return;
}

int main () {
    void (*f)();

    f = nondet () : &foo : &bar;
    f();

    return 0;
}
