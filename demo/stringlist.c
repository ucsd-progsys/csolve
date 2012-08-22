// The following program illustrates a variety of functions for manipulating
// Pascal-style strings and lists of such.
//
// All array accesses in this file are verified safe by CSolve.

#include <stdlib.h>
#include <csolve.h>

char * make_string (int n) {
    if (n <= 0)
        return 0;

    char *str0 = (char *) malloc (n * sizeof (char));
    char *str  = str0;

    for (int i = 0; i < n; i++) {
        *str++ = 0;
    }

    return str0;
}

// Inferred invariant: the length of each string pointed to by the str field
// is equal to the value of the len field.
typedef struct {
    int   len;
    char *str;
} string;

// CSolve uses the above invariant to infer that all the array accesses within
// this function are safe.
void init_string (string *s, char c) {
    for (int i = 0; i < s->len; i++) {
        s->str[i] = c;
    }
}

string *new_string (int n, char c) {
    string *s;
    char   *str;

    if (n <= 0)
        return 0;

    s      = (string *) malloc (sizeof (string));
    s->len = n;
    s->str = make_string (n);

    init_string (s, c);

    return s;
}

typedef struct _slist {
    struct _slist  * next;
    string * LOC(SL) s;
} slist;

slist *new_strings (int n) {
    string *s;
    slist  *sl, *t;
    string *str;

    sl = 0;
    for (int i = 1; i < n; i++) {
        s       = (string *) malloc (sizeof (string));
        s->len  = i;
        s->str  = make_string(i);

        t       = (slist *) malloc (sizeof (slist));
        t->s    = s;
        t->next = sl;
        sl      = t;
    }

    return sl;
}

// This function illustrates an idiom where a linked list header is stored as a
// "prefix" to a structure, so that the linked list portion can be accessed by
// jogging the pointer into the structure backwards.
//
// This requires that we annotate the parameter s to indicate that it points 4
// bytes into the structure, rather than at the beginning of the structure.
string * LOC(L) string_succ (slist INST(SL, L) * REF(V = (BLOCK_BEGIN([V]) + 4)) s) {
    slist *sl;

    if (s == (slist *) 0)
        return (slist *) 0;

    sl = (slist **)s - 1;
    sl = sl->next;

    if (sl == (slist *) 0)
        return (slist *) 0;

    return sl->s;
}

// Driver function
void main () {
    char *str = make_string (nondetpos ());
    new_string (nondetpos (), (char) nondetpos ());
    slist *sls = new_strings (nondetpos ());

    if (sls == (slist *) 0)
        return;

    string *s = string_succ(&sls->s);
}
