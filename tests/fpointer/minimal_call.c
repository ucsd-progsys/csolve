void nop () {
    return;
}

void doNothin (void (*f) ()) {
    f ();
}

int main () {
    doNothin (&f);
    
    return 0;
}
