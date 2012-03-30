#include <stdlib.h>
#include <csolve.h>

/******************************************************************/
/********************* Type Definitions ***************************/
/******************************************************************/

struct hash_entry {
   unsigned int key ;
   int entry ;		//JHALA POLY ISSUE
    // next doesn't get any of the useful properties that the first hash entry does,
    // even coming directly out of MakeHash and MakeGraph
    // this is where --notruekvars might be useful...
    // - what to do about untouched locations?
   struct hash_entry *next ;
   unsigned int padding ;
};

typedef struct hash_entry *HashEntry;

struct hash {
   HashEntry *  ARRAY array ;
   //int (*mapfunc)(unsigned int  ) ;
   int size ;
   unsigned int padding ;
};

typedef struct hash *Hash;

struct vert_st {
   int mindist ;
   struct vert_st * ARRAY next ;
   Hash LOC(HL) edgehash ;
   unsigned int padding ;
};

typedef struct vert_st * LOC(VL) ARRAY Vertex;

struct graph_st {
   //struct vert_st * ARRAY vlist ; //JHALA: each cell=0 or validptr into array
   Vertex vlist ; //JHALA: each cell=0 or validptr into array

};

typedef struct graph_st *Graph;

struct blue_return {
   Vertex INST(VL, VL) vert ;
   int dist ;
};

typedef struct blue_return *BlueReturn;

/******************************************************************/
/****************************** Code ******************************/
/******************************************************************/

Hash MakeHash(int size /* , int (*map)(unsigned int  ) */ )
{ Hash retval ;
  void *tmp ;
  void *tmp___0 ;
  {
    tmp = /* localmalloc */malloc((int )sizeof(*retval));
    retval = (struct hash *)tmp;
    retval->size = size;
    tmp___0 = /*localmalloc*/malloc((int )((unsigned int )size * sizeof(*(retval->array + 0))));
    retval->array = (HashEntry *)tmp___0;
    //NUKE memset((char *)retval->array, 0, (unsigned int )size * sizeof(*(retval->array + 0)));
    /* retval->mapfunc = map; */
    retval->padding = 0U;
    return (retval);
  }
}

static int hashfunc(/* JHALA: */unsigned int HashRange, unsigned int key )
{
  return ((int )((key >> 4) % (unsigned int )HashRange));
}


//void *HashLookup(unsigned int key , Hash hash ) JHALA POLY ISSUE
int HashLookup(unsigned int key , Hash hash )
{ int j ;
  HashEntry ent ;
  int a = csolve_assume(hash != (Hash)0);

  {
  // Necessary if we don't use --notruekvars - why only then?
  /* if (hash->size <= 0) return 0; */

  j = /*(*(hash->mapfunc))*/hashfunc(hash->size, key);
  csolve_assert(j >= 0);
  csolve_assert(j < hash->size);

  ent = *(hash->array + j);
  while (1) {
    if (ent) {
      //if (! (ent->key != key)) {	JHALA NOT NOT HANDLED IN cilInterface
      if ((ent->key == key)) {
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
  return (/*(void *)*/0); //JHALA POLY ISSUE
}
}


//void HashInsert(void *entry , unsigned int key , Hash hash )  JHALA POLY ISSUE
void HashInsert(int entry , unsigned int key , Hash hash )
{ HashEntry ent ;
  int j ;
  /* void *tmp; */
  int a = csolve_assume(hash != (Hash) 0); // pmr: needed for safe derefs to hash->

  {
  // csolve_assert(3,!HashLookup(key,hash));
  j = /*(*(hash->mapfunc))*/hashfunc(hash->size, key);
  csolve_assert(j>=0);
  csolve_assert(j<hash->size);
  /*tmp*/ent = /*localmalloc*/(HashEntry *) malloc((int )sizeof(*ent));
  //ent = (struct hash_entry *)tmp;
  ent->next = *(hash->array + j);
  *(hash->array + j) = ent;
  ent->key = key;
  ent->entry = entry;
  return;
}
}





static int mult(int p , int q )
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

static int mst_random(int seed )
{ int tmp ;
  int tmp___0 ;

  {
  tmp___0 = mult(seed, 31415821);
  tmp = tmp___0 + 1;
  return (tmp);
}
}

static int compute_dist(int i , int j , int numvert )
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


static void AddEdges(Graph retval , int numvert )
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
    src = retval->vlist + j;
    hash = src->edgehash;
    i = 0;
    while (i < numvert) {
      if (i != j) {
        dist = compute_dist(i, j, numvert);
        dest = retval->vlist + i;
        HashInsert(/* JHALA POLY ISSUE (void *) */dist, (unsigned int )dest, hash);
        num_inserted ++;
      }
      i ++;
    }
    j ++;
  }
  //chatting((char *)"%d edges inserted\n", num_inserted);
  return;
}
}

Graph MakeGraph(int numvert )
{ int i ;
  Vertex vf ;
  Vertex vt ;
  Graph retval ;
  //int tmp ;		JHALA: bogus int->ptr cast
  //int tmp___0 ;	JHALA:
  int HashRange; 	//JHALA: making local

  {
  HashRange = numvert / 4; //JHALA: hoisted from loop-body
  //tmp = malloc(sizeof(*retval));
  //retval = (struct graph_st *) tmp;
  retval = (struct graph_st *) malloc(sizeof(*retval));
  //chatting((char *)"Make phase 1: Creating hash tables\n");
  //tmp___0 = malloc((unsigned int )numvert * sizeof(*vf));
  //retval->vlist = (struct vert_st *)tmp___0;
  retval->vlist = (struct vert_st *) malloc((unsigned int )numvert * sizeof(*vf));
  vt = (struct vert_st *)0;
  i = numvert - 1;
  while (i >= 0) {
    vf = retval->vlist + i;
    vf->mindist = 9999999;
    vf->edgehash = MakeHash(HashRange/*, & hashfunc*/);
    vf->next = vt;
    vt = vf;
    i --;
  }
  //chatting((char *)"Make phase 3: Creating graph\n");
  AddEdges(retval, numvert);
  //chatting((char *)"Make returning\n");
  return (retval);
}
}

static struct blue_return INST(VL, L) *BlueRule(Vertex INST(VL, L) inserted , Vertex INST(VL, L) vlist )
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

  retval = malloc(sizeof(*retval)); 	//JHALA
  {
  if (/* ! vlist */ vlist == (Vertex) 0) {	//JHALA UNOP REFERENCE ISSUE
    retval->dist = 999999; 		//JHALA
    return (retval);
  }
  prev = vlist;
  retval->vert = vlist;			//JHALA
  retval->dist = vlist->mindist;	//JHALA
  hash = vlist->edgehash;
  // JHALA POLY ISSUE
  //tmp___0 = HashLookup((unsigned int )inserted, hash);
  //dist = (int )tmp___0;
  dist = HashLookup((unsigned int )inserted, hash);
  if (dist) {
    if (dist < retval->dist) {		//JHALA
      vlist->mindist = dist;
      retval->dist = dist;		//JHALA
    }
  } else {
   // chatting((char *)"Not found\n");
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
      // JHALA POLY ISSUE
      // tmp___1 = HashLookup((unsigned int )inserted, hash);
      // dist = (int )tmp___1;
      dist = HashLookup((unsigned int )inserted, hash);
      if (dist) {
        if (dist < dist2) {
	  tmp->mindist = dist;
          dist2 = dist;
        }
      } else {
        //chatting((char *)"Not found\n");
      }
      if (dist2 < retval->dist) {		//JHALA
        retval->vert = tmp;			//JHALA
        retval->dist = dist2;			//JHALA
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
  BlueReturn br ;

  {
  Vertex MyVertexList  =    (Vertex )0;
  cost = 0;
  //chatting((char *)"Compute phase 1\n");
  inserted = graph->vlist;
  tmp = inserted->next;
  //graph->vlist = tmp;			       //JHALA: Gratuitous assignment! wrecks validptr(inserted)
  					       //could be solved if we knew graph pointed to conc
  MyVertexList = tmp;
  numvert --;
  //chatting((char *)"Compute phase 2\n");
  while (numvert) {
    if ((unsigned int )inserted == (unsigned int )MyVertexList) {
      int assm = csolve_assume(inserted != (Vertex) 0);	//JHALA numvert = listlength...
      MyVertexList = MyVertexList->next;
    }
    br = BlueRule(inserted, MyVertexList);
    inserted = br->vert;
    dist = br->dist;
    numvert --;
    cost += dist;
  }
  return (cost);
}
}

int dealwithargs(int argc , char * ARRAY VALIDPTR * START NONNULL ARRAY SIZE(argc * 4) argv)
{ int level ;
  if (argc > 1) {
    level = atoi(*(argv + 1));
  } else {
    level = 1024;
  }
  return (level);
}

int main(int argc, char NULLTERMSTR * ARRAY VALIDPTR * START NONNULL ARRAY SIZE(argc * 4) argv) CHECK_TYPE {
  Graph graph;
  int dist ;
  int size ;
  {
  size  = dealwithargs(argc, argv);
  if (size >= 4){                        //JHALA: Otherwise NN-error!
                                         // pmr: Otherwise HashRange may be zero
    graph = MakeGraph(size);
    dist  = ComputeMst(graph, size);
  }
  exit(0);
  return 0;
}
}

