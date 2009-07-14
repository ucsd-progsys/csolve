typedef unsigned int size_t;
extern char *malloc(int);
extern void *memset(void *, int, size_t);
extern void exit(int);

struct hash_entry {
   unsigned int key ;
   int entry ;
   struct hash_entry *next ;
   unsigned int padding ;
};
typedef struct hash_entry *HashEntry;

struct hash {
   int size ;
   HashEntry *array ;
};
typedef struct hash *Hash;

struct vert_st {
   int mindist ;
   struct vert_st *next ;
   Hash edgehash ;
    int pad;
};
typedef struct vert_st *Vertex;

struct graph_st {
   int numvert ;
   Vertex vlist ;
};
typedef struct graph_st *Graph;

struct blue_return {
   Vertex vert ;
   int dist ;
};
typedef struct blue_return BlueReturn;

int dealwithargs(int argc , char **argv )
{ int level ;

  {
  if (argc > 1) {
    level = atoi((char const   *)*(argv + 1));
  } else {
    level = 1024;
  }
  return (level);
}
}

int mult(int p , int q )
{ int p1 ;
  int p0 ;
  int q1 ;
  int q0 ;

  {
  p1 = p / 10000;
  p0 = p % 10000;
  q1 = q / 10000;
  q0 = q % 10000;
  return (((p0 * q1 + p1 * q0) % 10000) * 10000 + p0 * q0);
}
}

int mst_random(int seed )
{ int tmp ;
  int tmp___0 ;

  {
  tmp___0 = mult(seed, 31415821);
  tmp = tmp___0 + 1;
  return (tmp);
}
}

int compute_dist(int i , int j , int numvert )
{ int less ;
  int gt ;
  int tmp ;

  {
  if (i < j) {
    less = i;
    gt = j;
  } else {
    less = j;
    gt = i;
  }
  tmp = mst_random(less * numvert + gt);
  return (tmp % 2048 + 1);
}
}

int hashfunc(unsigned int key )
{
  return ((int )((key >> 4) % 100U));
}

Hash MakeHash(int size )
{ Hash retval ;
  void *tmp ;
  void *tmp___0 ;
  HashEntry e;
  Hash rt;
  int ndp;

  {
  tmp = malloc(sizeof(*retval));
  retval = (struct hash *)tmp;
  retval->size = size;
  tmp___0 = malloc((unsigned int )size * sizeof(*(retval->array + 0)));
  retval->array = (HashEntry *)tmp___0;
  // memset((void *)((char *)retval->array), 0, (unsigned int )size * sizeof(*(retval->array + 0)));
  // pmr: the following is a bogus "approximation" to memset
  // pmr: we'll need to initialize all these fields somehow
  for (int i = 0; i < size; i++) {
      retval->array[nondet()] = (HashEntry)malloc(sizeof(struct hash_entry));
      e                = retval->array[i];
      ndp              = nondetpos (); // sloc_of_ret ISSUE
      e->key           = ndp;
      e->padding       = 0;
      e->entry         = ndp;
      e->next          = e;
  }

  return retval;
}
}

int HashLookup(unsigned int key , Hash hash )
{ int j ;
  HashEntry ent ;

  {
  j = hashfunc(key);
  ent = hash->array[j];
  while (1) {
    if (ent) {
      if (! (ent->key != key)) {
        break;
      }
    } else {
      break;
    }
    ent = ent->next;
  }
  if (ent) {
    return (ent->entry);
  }
  return 0;
}
}

void HashInsert(int entry, unsigned int key, Hash hash )
{ HashEntry ent ;
  int j ;
  void *tmp ;

  {
  j = hashfunc(key);
  tmp = malloc(sizeof(*ent));
  ent = (struct hash_entry *)tmp;
  ent->next = hash->array[j];
  hash->array[j] = ent;
  ent->key = key;
  ent->entry = entry;
  return;
}
}

void AddEdges(Graph retval, int numvert)
{ Vertex src ;
  Vertex dest ;
  Hash hash ;
  int i ;
  int j ;
  int dist ;
  int num_inserted ;

  {
  num_inserted = 0;
  j = 0;
  while (j < numvert) {
      src = &(retval->vlist[j]);
    hash = src->edgehash;
    i = 0;
    while (i < numvert) {
      if (i != j) {
        dist = compute_dist(i, j, numvert);
        dest = &(retval->vlist[i]);
        HashInsert(dist, (unsigned int )dest, hash);
        num_inserted ++;
      }
      i ++;
    }
    j ++;
  }
  return;
}
}

Graph MakeGraph(int numvert )
{ int i ;
  Vertex vf ;
  Vertex vt ;
  Graph retval ;
  void *tmp ;
  void *tmp___0 ;
  Hash htmp;

  {
  tmp = malloc(sizeof(*retval));
  retval = (struct graph_st *)tmp;
  retval->numvert = numvert;
  tmp___0 = malloc((unsigned int )numvert * sizeof(*vf));
  retval->vlist = (struct vert_st *)tmp___0;
  vt = (struct vert_st *)((void *)0);
  for (i = 0; i < numvert; i++) {
    vf = retval->vlist + i;
    vf->mindist = 9999999;
    htmp = MakeHash(100);
    vf->edgehash = htmp; // sloc_of_ret ISSUE
  }
  for (i = 0; i < numvert; i++) {
      vf = retval->vlist + i;
      vf->next = retval->vlist + i + 1;
  }
  vf->next = 0;
  AddEdges(retval, numvert);
  return (retval);
}
}

/*
static BlueReturn BlueRule(Vertex inserted , Vertex vlist )
{ BlueReturn retval ;
  Vertex tmp ;
  Vertex prev ;
  Hash hash ;
  int dist ;
  int dist2 ;
  int count ;
  void *tmp___0 ;
  Vertex next ;
  void *tmp___1 ;

  {
  if (! vlist) {
    retval.dist = 999999;
    return (retval);
  }
  prev = vlist;
  retval.vert = vlist;
  retval.dist = vlist->mindist;
  hash = vlist->edgehash;
  tmp___0 = HashLookup((unsigned int )inserted, hash);
  dist = (int )tmp___0;
  if (dist) {
    if (dist < retval.dist) {
      vlist->mindist = dist;
      retval.dist = dist;
    }
  }
  count = 0;
  tmp = vlist->next;
  while (tmp) {
    count ++;
    if ((unsigned int )tmp == (unsigned int )inserted) {
      next = tmp->next;
      prev->next = next;
    } else {
      hash = tmp->edgehash;
      dist2 = tmp->mindist;
      tmp___1 = HashLookup((unsigned int )inserted, hash);
      dist = (int )tmp___1;
      if (dist) {
        if (dist < dist2) {
          tmp->mindist = dist;
          dist2 = dist;
        }
      }
      if (dist2 < retval.dist) {
        retval.vert = tmp;
        retval.dist = dist2;
      }
    }
    prev = tmp;
    tmp = tmp->next;
  }
  return (retval);
}
}
static int ComputeMst(Graph graph , int numvert )
{ Vertex inserted ;
  Vertex tmp ;
  int cost ;
  int dist ;
  Vertex MyVertexList ;
  BlueReturn br ;

  {
  cost = 0;
  MyVertexList = (Vertex )((void *)0);
  inserted = graph->vlist;
  tmp = inserted->next;
  graph->vlist = (struct vert_st *)((void *)0);
  graph->numvert = 1;
  graph->vlist = tmp;
  MyVertexList = tmp;
  numvert --;
  while (numvert) {
    if ((unsigned int )inserted == (unsigned int )MyVertexList) {
      MyVertexList = MyVertexList->next;
    }
    br = BlueRule(inserted, MyVertexList);
    inserted = br.vert;
    dist = br.dist;
    numvert --;
    cost += dist;
  }
  return (cost);
}
}
int main(int argc , char **argv )
{ Graph graph ;
  int dist ;
  int size ;

  {
  size = dealwithargs(argc, argv);
  graph = MakeGraph(size);
  dist = ComputeMst(graph, size);
  exit(0);
}
}
*/
