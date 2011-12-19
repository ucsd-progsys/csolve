#include <csolve.h>

typedef struct _node {
    struct _node *next;
} node;

void break_it (node ** NONNULL npp, node *v) CHECK_TYPE {
    if (0 == 1) {
        v->next = *npp;
    }
}
