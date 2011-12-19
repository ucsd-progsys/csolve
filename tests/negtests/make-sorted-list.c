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
    node *cur = (node *) 0;
    node *prev = (node *) 0;

    for (int i = 0; i < 10; i++) {
        cur       = (node *) malloc (sizeof (node));
        cur->val  = i;
        cur->next = prev;
        prev      = cur;
    }

    test_sorted (cur);

    return 0;
}
