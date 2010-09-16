extern void *malloc (int);

typedef struct _node {
    // Craps out in inferctypes if val comes first!
    struct _node *next;
    int val;
} node;

void insert (node **hd, node *v) {
    node **_e_;

    // Loop exploded into following while for reasons that will soon become clear...
    /* for (_e_ = hd; */
    /*      *_e_ != (node *) 0 /\* && (*_e_)->val < v->val *\/; */
    /*      _e_ = &((*_e_)->next)) */
    /*     ; */

    _e_ = hd;
    while (1) {
        // We need to factor out this constant subexpression because we
        // don't have "local" finality, i.e., can't let a field be final for
        // part of a function.  In particular, the next field that _e_ points
        // to is not final, so we can't use DEREF(_e_) in a refinement.
        node *ptr = *_e_;

        if (ptr == (node *) 0)
            break;

        if (ptr->val >= v->val)
            break;

        _e_ = &(ptr->next);
    }

    /* Chokes - WTF? */
    /* v->next = *_e_; */
    node *tmp;
    tmp     = *_e_;
    v->next = tmp;

    *_e_ = v;
}

int main () {
    node hd;

    while (1) {
        node *v = (node *) malloc (sizeof (node));
        v->val  = nondet ();
        v->next = (node *) 0;
        insert (&hd, v);
    }

    return 0;
}
