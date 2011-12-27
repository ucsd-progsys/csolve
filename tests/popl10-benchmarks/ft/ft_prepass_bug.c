#include <csolve.h>

struct Vertices {
   int key ;
};

struct Heap {
   struct Vertices * LOC(IL) item ;
};

int LessThan(struct Vertices * LOC(L) item1)
{ 
    return (item1->key < item1->key);
}

// If these are K, not L, it's ok - see note below
// Shape mismatch? Inlining doesn't seem to work here...
struct Heap INST(IL, IL) * LOC(L) Insert(struct Heap * LOC(L) h, struct Vertices * LOC(IL) i)
{
  LessThan(h->item);
  return (0);
}

// w/ fresh location instead of L it's ok - is some capture happening here?
// is there a location that's not closed in the spec?
//   no - all specs are closed
void MST(struct Vertices * LOC(L) graph) 
{
  Insert(0, graph);
}
