typedef int (*fptr) ();

void call_it (fptr f) {
    int z = f();
    assert (z > 0);
}
