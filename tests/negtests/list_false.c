#include <stdlib.h>

typedef struct _node {
    struct _node *next;
} node;

void insert (node * LOC(L) * LOC(L) hd, node * LOC(L) v) {
    node **_e_;

    /* lcc_assert (0); */
    _e_  = hd;
    *_e_ = v;

    lcc_assert (0);
}

int main () {
    node hd;

    insert (&hd, &hd);

    return 0;
}
