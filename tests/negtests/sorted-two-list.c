#include <stdlib.h>

typedef struct _node {
    int           val;
    struct _node *next;
} node;

void test (node *hd) {
    if (hd->next != (node *) 0) {
        csolve_assert (hd->val < hd->next->val);
    }

    return;
}

int main () {
    node *hd = (node *) malloc (sizeof (node));
    hd->val  = nondet ();
    hd->next = (node *) 0;

    node *nxt = (node *) malloc (sizeof (node));
    nxt->val  = nondet ();
    nxt->next = (node *) 0;

    if (hd->val <= nxt->val) {
        hd->next = nxt;
    }

    test (hd);

    return 0;
}
