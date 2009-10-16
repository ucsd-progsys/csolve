extern int nondet();
extern char *malloc(int);

typedef struct __thing {
    int n;
    char s[10];
} thing;

void main () {
    unsigned char u;
    thing t;

    u = (unsigned char) nondet();

    t.n    = 15;
    t.s[u] = 16;
}
