

#include <stdlib.h>
#include <csolve.h>

char * ARRAY make_string(int n) {
    if (n <= 0)
        return 0;

    char *str0 = (char *)malloc(n * sizeof(char));
    char *str  = str0;

    for (int i = 0; i < n; i++) {
        *str++ = 0;
    }

    return str0;
}

typedef struct {
    int  len;
    char * ARRAY str;
} string;

void init_string(string *s, char c) {
    for (int i = 0; i < s->len; i++) {
        s->str[i] = c;
    }
}

string *new_string(int n, char c) {
    string *s;
    char   *str;

    if (n <= 0)
        return 0;

    s      = (string *)malloc(sizeof(string));
    s->len = n;
    s->str = make_string(n);

    init_string(s, c);

    return s;
}

typedef struct _slist {
    struct _slist  * next;
    string * LOC(SL) s;
} slist;

slist *new_strings(int n) {
    string *s;
    slist  *sl, *t;
    string *str;

    sl = 0;
    for (int i = 1; i < n; i++) {
        s       = (string *)malloc(sizeof(string));
        s->len  = i;
        s->str  = make_string(i);

        t       = (slist *)malloc(sizeof(slist));
        t->s    = s;
        t->next = sl;
        sl      = t;
    }

    return sl;
}

string * LOC(L) string_succ(slist INST(SL, L) * REF(V = (BLOCK_BEGIN([V]) + 4)) s) {
    slist *sl;

    if (s == (slist *) 0)
        return (slist *) 0;

    sl = (slist **)s - 1;


    csolve_validptr_lo (sl);
    csolve_validptr_hi (sl); 
    sl = sl->next;

    if (sl == (slist *) 0)
        return (slist *) 0;

    return sl->s;
}

void main () {
    char *str = make_string(nondetpos());
    new_string (nondetpos(), (char)nondetpos());
    slist *sls = new_strings(nondetpos());

    if (sls == (slist *) 0)
        return;

    string *s = string_succ(&sls->s);
    // csolve_assert(0); // Sanity
    // init_string(s, 0);
}
