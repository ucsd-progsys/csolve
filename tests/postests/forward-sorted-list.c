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
  node *cur = (node *) malloc (sizeof (node));
  cur->val  = 0;
  cur->next = (node *) 0;

  node *hd = cur;
  for (int i = 1; i < 10; i++) {
    node *n   = (node *) malloc (sizeof (node));
    n->val    = i;
    n->next   = (node *) 0;
    cur->next = n;
    cur       = n;
  }

  test_sorted (hd);

  return 0;
}
