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

// glib's s_list copy with noted modifications
node *copy (node *hd) {
    node *nnew = (node *) 0;

    if (hd) {
        node *last;

        nnew       = (node *) malloc (sizeof (node));
        nnew->val  = hd->val;
        nnew->next = (node *) 0;
        last       = nnew;
        hd         = hd->next;
        while (hd) {
            // changed to kill in-place alloc (i.e., allocates to cur->next in glib)
            node *cur  = (node *) malloc (sizeof (node));
            cur->val   = hd->val;
            cur->next  = (node *) 0;
            last->next = cur;
            last       = cur;
            hd         = hd->next;
        }
    }

    return nnew;
}

int main () {
    node *cur  = (node *) 0;
    node *prev = (node *) 0;

    for (int i = nondetnn (); i >= 0; i--) {
        cur       = (node *) malloc (sizeof (node));
        cur->val  = i;
        cur->next = prev;
        prev      = cur;
    }

    test_sorted (cur);
    test_sorted (copy (cur));

    return 0;
}
