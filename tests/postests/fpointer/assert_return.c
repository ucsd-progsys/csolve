int two () {
    return 2;
}

int main () {
    int (*f) () = &two;
    int x = f ();
    assert (x > 0);

    return 0;
}
