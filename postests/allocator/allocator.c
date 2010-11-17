extern void *malloc (int);

#define NULL        0
#define ALLOC_COUNT 10
#define ARRAY       __attribute__ ((__array__))

struct region_struct {
    int                   size;
    char                 * ARRAY mem;
    struct region_struct *next;
};

typedef struct region_struct region;

struct pool_struct {
    int                 size;
    region             *free;
    struct pool_struct *next;
};

typedef struct pool_struct free_pool;

typedef struct {
    free_pool *free;
    region    *allocated;
} allocator_state;

char * ARRAY pool_allocate (allocator_state *state, free_pool *p) {
    if (p->free) return p->free->mem;

    char *mem = (char *) malloc (p->size * ALLOC_COUNT);
    region *r = NULL;
    for (int i = 0; i < ALLOC_COUNT; i++) {
        r       = (region *) malloc (sizeof (region));
        r->size = p->size;
        r->mem  = mem;
        r->next = p->free;
        p->free = r;
        mem    += p->size;
    }

    int a = assume (r != NULL);
    r->next          = state->allocated;
    state->allocated = r;

    return r->mem;
}

char * ARRAY alloc (allocator_state *state, int size) {
    if (size <= 0) return NULL;

    free_pool *p;
    for (p = state->free; p->size < size && p->next != NULL; p = p->next) ;

    // p->size >= size || p->next == NULL
    if (p->size >= size) return pool_allocate (state, p);

    // p->next == NULL
    free_pool *np = (free_pool *) malloc (sizeof (free_pool));
    np->size      = 2 * p->size;
    np->free      = NULL;
    np->next      = NULL;
    p->next       = np;

    return pool_allocate (state, np);
}

void dealloc (allocator_state *state, char * ARRAY mem) {
    if (mem == NULL) return;

    region *r, *prev = NULL;
    for (r = state->allocated; r != mem && r != NULL; r = r->next, prev = r) ;

    if (r == NULL) return;

    if (prev != NULL) prev->next = r->next;

    free_pool *p;
    for (p = state->free; p != NULL && p->size != r->size; p = p->next) ;

    if (p == NULL) return;

    r->next = p->free;
    p->free = r;
}

void main () {
    free_pool *p = (free_pool *) malloc (sizeof (free_pool));
    p->size      = 1024;
    p->free      = NULL;
    p->next      = NULL;

    allocator_state *state = (allocator_state *) malloc (sizeof (allocator_state));
    state->allocated       = NULL;
    state->free            = p;

    char *m = NULL;
    while (1) {
        if (nondet ()) {
            m = alloc (state, nondet ());
        } else {
            dealloc (state, m);
            m = NULL;
        }
    }
}
