extern void *malloc (int);
#define NULL 0

struct region_struct {
    struct region_struct *next;
    int                   size;
    char                  mem[0];
};

typedef struct region_struct region;

struct pool_struct {
    int                 size;
    region             *free;
    struct pool_struct *next;
};

typedef struct pool_struct free_pool;

char *pool_alloc (free_pool *freelist, free_pool *p) {
    // pmr : HEREHERE
    /* if (p->free) return &p->free->mem; */

    // change to sbrk
    region *r = (region *) malloc (sizeof (region) + p->size);
    r->next   = NULL;
    r->size   = p->size;

    char *mem = &r->mem;
    for (int i = 0; i < p->size; i++) {
        *mem = 0;
        mem++;
    }

    return &r->mem;
}

char *alloc (free_pool *freelist, int size) {
    if (size <= 0) return NULL;

    free_pool *p;
    for (p = freelist; p->size < size && p->next != NULL; p = p->next) ;

    // p->size >= size || p->next == NULL
    if (p->size >= size) return pool_alloc (freelist, p);

    // p->next == NULL
    // change to sbrk
    free_pool *np = (free_pool *) malloc (sizeof (free_pool));
    np->size      = 2 * p->size;
    np->free      = NULL;
    np->next      = NULL;
    p->next       = np;

    return pool_alloc (freelist, np);
}

void dealloc (free_pool *freelist, char *mem) {
    if (mem == NULL) return;

    region *r = (region *) mem - 1;

    free_pool *p;
    for (p = freelist; p != NULL && p->size != r->size; p = p->next) ;

    if (p == NULL) return;

    r->next = p->free;
    p->free = r;
}

void main () {
    free_pool *fl = (free_pool *) malloc (sizeof (free_pool));
    fl->size      = 1024;
    fl->free      = NULL;
    fl->next      = NULL;

    void *m = NULL;
    while (1) {
        if (nondet ()) {
            m = alloc (fl, nondet ());
        } else {
            dealloc (fl, m);
            m = NULL;
        }
    }
}
