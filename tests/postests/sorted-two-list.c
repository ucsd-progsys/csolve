#include <stdlib.h>

typedef struct _node {
    int           val;
    struct _node *next;
} node;

void test_sorted (node *hd) {
    node *cur = hd;

    while (cur != (node *) 0 && cur->next != (node *) 0) {
        csolve_assert (cur->val < cur->next->val);
        cur = cur->next;
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

    if (hd->val < nxt->val) {
        hd->next = nxt;
    }

    test_sorted (hd);

    return 0;
}
