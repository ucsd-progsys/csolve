#ifndef __LIQUIDC_H__
#define __LIQUIDC_H__

#define ARRAY __attribute__ ((array))

int assume (int p) {
    if (!p)
    STUCK: goto STUCK;

    return p;
}

int bor(int a, int b) {
    int res;

    res = a | b;

    if (a > res) goto STUCK;
    if (b > res) goto STUCK;
    if (res > a + b) goto STUCK;

    return res;

 STUCK: goto STUCK;
}

int band(int a, int b) {
    int res;

    if (b < 0)
        exit (1);

    res = a & b;

    if (res > a) goto STUCK;
    if (res > b) goto STUCK;
    if (res < 0) goto STUCK;

    return res;

 STUCK: goto STUCK;
}

#endif
