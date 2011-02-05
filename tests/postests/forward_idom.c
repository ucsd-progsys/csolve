void main () {
    int n;

    goto next;

    backhere:
    n = 0;

    next:
    goto backhere;
}
