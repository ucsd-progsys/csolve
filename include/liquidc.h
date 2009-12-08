#ifndef __LIQUIDC_H__
#define __LIQUIDC_H__

#define ARRAY __attribute__ ((array))

/* Phony operators for desirable properties, e.g., in ADPCM */
extern int bor(int, int);
extern int band(int, int);

int assume (int p) {
    if (!p)
    STUCK: goto STUCK;

    return 1;
}

#endif
