extern char *malloc(int);

typedef struct substruct {
    int y;
} substruct_t;

typedef struct lstruct {
    int x;
    substruct_t s;
} lstruct_t;

void f() {
    lstruct_t ls;
    substruct_t ss;

    ls.x = 0;
    ls.s.y = 1;

    ss = ls.s;
}
