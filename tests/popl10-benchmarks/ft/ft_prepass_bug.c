#include <csolve.h>

struct Vertices {
   int key;
};

struct Heap {
   struct Vertices * LOC(IL) i;
};

// If these are K, not L, it's ok - see note below
struct Heap INST(IL, IL) * LOC(L) insert (struct Heap * LOC(L) h, struct Vertices * LOC(IL) i) {
  int k = h->i->key;
  return 0;
}

// w/ fresh location instead of L it's ok - is some capture happening here?
// is there a location that's not closed in the spec?
//   no - all specs are closed
// loc shouldn't figure in to shape inference post-specgen!
// why is this not fresh? or is it?
// substs on locations should be disjoint but they wind up not being so because
// of name reuse...?!
// note new subst is simultaneous rather than extended,
// but not getting treated like it...?
void mst (struct Vertices * LOC(L) graph) {
  insert (0, graph);
}
