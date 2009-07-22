extern char *malloc(int);

typedef struct s {
    int *uninit;
} s;

s *alloc() {
    return (s *)malloc(sizeof(s));
}

void access(s *st)
{
    st = st;
    validptr(st->uninit);
    assert(0);
}

void main()
{ 
    s *mst;

    mst = alloc();
    access(mst);
}
