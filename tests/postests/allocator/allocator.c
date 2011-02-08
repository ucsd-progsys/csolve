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

void init (int size, char *m) {
    while (size--) *m++ = 0;
}

char *new_region (int size) {
    region *r = (region *) malloc (sizeof (region) + size);
    r->next   = NULL;
    r->size   = size;
    init (size, r->mem);
    return &r->mem;
}

char *pool_alloc (free_pool *p) {
    if (p->free) {
        region *r = p->free;
        p->free   = r->next;

        return &r->mem;
    }

    return new_region (p->size);
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
    init (r->size, r->mem);

    free_pool *p = freelist;
    while (p->size != r->size) {
        p = p->next;
        if (p == NULL) return;
    }

    r->next = p->free;
    p->free = r;
}

void check_invariants (free_pool *fl) {
    for (free_pool *p = fl; p != NULL; p = p->next) {
        if (p->next != NULL) assert (p->size < p->next->size);
        if (p->free != NULL) assert (p->free->size == p->size);
        for (region *r = p->free; r != NULL; r = r->next) {
            if (r->next != NULL) assert (r->size == r->next->size);
            // validptr kills finality because we don't know what effect it may have
            // Hack around silly bug in inferindices
            int off = r->size - 1;
            char *mem = &r->mem;
            mem[off] = 0;
        }
    }
}

void main () {
    free_pool *fl = (free_pool *) sbrk (sizeof (free_pool));
    fl->size      = 1024;
    fl->free      = NULL;
    fl->next      = NULL;

	//RJ: Avoid 0 \lub 8 pollution
    //void *m = NULL;
    
    void *m = alloc (fl, nondet ());
	
	while (1) {
        if (nondet ()) {
            m = alloc (fl, nondet ());
        } else {
            dealloc (fl, m);
        }

        check_invariants (fl);
    }
}