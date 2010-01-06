#ifndef __LIQUIDC_H__
#define __LIQUIDC_H__

#define ARRAY __attribute__ ((array))

int assume (int p) {
    if (!p)
    STUCK: goto STUCK;

    return 1;
}

int bor(int a, int b) {
    int res;

    res = a | b;

    int a1 = assume (a <= res);
    int a2 = assume (b <= res);
    int a3 = assume (res <= a + b);

    return res;
}

int band(int a, int b) {
    int res;

    if (b < 0)
        exit (1);

    res = a & b;

    int a1 = assume (res <= a);
    int a2 = assume (res <= b);
    int a3 = assume (0 <= res);

    return res;
}

#endif
