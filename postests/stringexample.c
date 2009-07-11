extern char *malloc(int);

char *make_string(int n) {
    if (n <= 0)
        return 0;

    char *str0 = (char *)malloc(n * sizeof(char));
    char *str  = str0;

    for (int i = 0; i < n; i++) {
        validptr(str);
        *str++ = 0;
    }

    return str0;
}

typedef struct {
    int  len;
    char *str;
} string;

void init_string(string *s, char c) {
    int len;
    char *str;

    s = s; // THETA ISSUE

    // pmr: weirdness here, these need to be rolled out...
    len = s->len;
    str = s->str;
    for (int i = 0; i < len; i++) {
        validptr(str + i);
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
    str    = make_string(n);
    s->str = str;

    init_string(s, c);
    // assert(0); // Sanity

    return s;
}

typedef struct _slist {
    struct _slist *next;
    string        *s;
} slist;

slist *new_strings(int n) {
    string *s;
    slist  *sl, *t;
    string *str;

    sl = 0;
    for (int i = 1; i < n; i++) {
        s      = (string *)malloc(sizeof(string));
        s->len = i;
        str    = make_string(i); // sloc_of_ret ISSUE
        s->str = str;

        t       = (slist *)malloc(sizeof(slist));
        t->s    = s;
        t->next = sl;
        sl      = t;
    }

    return sl;
}

slist *string_succs(string **s) {
    slist *sl;

    sl = ((slist **)s) - 1;

    return sl->next;
}

void main () {
    char *str = make_string(nondetpos());
    new_string (nondetpos(), (char)nondetpos());
    slist *sls = new_strings(nondetpos());

    slist *sl2 = string_succs(&sls->s);
    string *s  = sl2->s;
    // assert(0); // Sanity
    init_string(s, 0);
}
