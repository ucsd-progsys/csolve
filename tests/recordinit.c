typedef struct list {
    struct list *next;
    struct list *prev;
} list_t;

typedef struct record {
    int    data1;
    list_t node;
    int    data2;
} record_t;

#define container(p) ((record_t*)((int*)(p) - 1))

void init_record(list_t *p) {
    record_t *r = container(p);
    r->data2    = 42;
}

void init_all_records(list_t *p) {
    while (p != NULL) {
        init_record(p);
        p = p->next;
    }
}
