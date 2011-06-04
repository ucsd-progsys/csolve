int two () {
    return 0;
}

int main () {
    int (*f) () = &two;
    int x = f ();
    assert (x > 0);

    return 0;
}
