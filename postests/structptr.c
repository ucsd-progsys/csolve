extern char *malloc(int);

typedef struct __str__ {
    int x;
    int y;
} str;

void foo (str *t) {
    if (t == (str *)0) { DIVERGE: goto DIVERGE; }
    t->x = 0;
    validptr(&(t->y));
}

void main () {
    str *s;

    s = nondet() ? (str *)malloc(sizeof(str)) : (str *)0;
    s->x = 0;
    foo(s);
    
}
