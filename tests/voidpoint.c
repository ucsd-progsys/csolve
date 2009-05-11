void main(void* vp) {
    int *ip;

    ip = (int*) vp;
    ip++;
    *ip = 200;

    return;
}
