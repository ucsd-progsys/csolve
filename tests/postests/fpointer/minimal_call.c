void nop () {
    return;
}

void doNothin (void (*f) ()) {
    f ();
}

int main () {
    doNothin (&nop);
    
    return 0;
}
