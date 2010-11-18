extern void *malloc (int);
// ;)
#define sbrk malloc
#define NULL 0

struct region_struct {
    int                   size;
    struct region_struct *next;
    char                  mem[0];
};

typedef struct region_struct region;

struct pool_struct {
    int                 size;
    region             *free;
    struct pool_struct *next;
};

typedef struct pool_struct free_pool;

char *pool_alloc (free_pool *p) {
    if (p->free) return &p->free->mem;

    region *r = (region *) malloc (sizeof (region) + p->size);
    r->next   = NULL;
    r->size   = p->size;

    for (int i = 0; i < p->size; i++)
        r->mem[i] = 0;

    return &r->mem;
}

char *alloc (free_pool *freelist, int size) {
    if (size <= 0) return NULL;

    free_pool *p;
    for (p = freelist;
         p->size < size && p->next != NULL;
         p = p->next) ;

    // p->size >= size || p->next == NULL
    if (p->size >= size) return pool_alloc (p);

    // p->next == NULL
    free_pool *np = (free_pool *) sbrk (sizeof (free_pool));
    np->size      = 2 * p->size;
    np->free      = NULL;
    np->next      = NULL;
    p->next       = np;

    return pool_alloc (np);
}

void dealloc (free_pool *freelist, char *mem) {
    if (mem == NULL) return;

    region *r    = (region *) mem - 1;
    free_pool *p = freelist;
    while (p->size != r->size) {
        p = p->next;
        if (p == NULL) return;
    }

    r->next = p->free;
    p->free = r;
}

void main () {
    free_pool *fl = (free_pool *) sbrk (sizeof (free_pool));
    fl->size      = 1024;
    fl->free      = NULL;
    fl->next      = NULL;

    void *m = NULL;
    while (1) {
        if (nondet ()) {
            m = alloc (fl, nondet ());
        } else {
            dealloc (fl, m);
        }

        for (free_pool *p = fl; p != NULL; p = p->next) {
            if (p->next != NULL) assert (p->size < p->next->size);

            for (region *r = p->free; r != NULL; r = r->next) {
                /* assert (r->size == p->size); */
            }
        }
    }
}
