// Test that a heapified global gets the correct initializer

int x = 5;

void main () {
    int *y = &x;

    assert (*y == 5);
}
