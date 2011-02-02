void main() {
    int x;
    int y;
    int flag;
    
    flag = 1;

    while (nondet() || flag) {
        flag = 0;
        x = 0;
    }

    assert (x == 0);

    return;
}
