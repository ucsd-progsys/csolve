typedef struct _node {
    struct _node *next;
} node;

void break_it (node **npp, node *v) {
    if (0 == 1) {
        v->next = *npp;
    }
}
