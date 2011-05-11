extern void *malloc (int);

int *domination = 0;

void incGlobal () {
    (*domination)++;
}

int main () {
    void (*f) ();

    domination = (int *) malloc (sizeof (int));
    f = &incGlobal;
    f ();

    return 0;
}
