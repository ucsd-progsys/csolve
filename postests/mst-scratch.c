/******************************************************************/
/********************* Type Definitions ***************************/
/******************************************************************/

typedef char *__attribute__((array)) *__attribute__((array)) string_array;

struct hash_entry {
   unsigned int key ;
   int entry ;		//JHALA POLY ISSUE
   struct hash_entry *next ;
   unsigned int padding ;
};

typedef struct hash_entry *HashEntry;

struct hash {
   HashEntry * __attribute__((array)) array ;
   //int (*mapfunc)(unsigned int  ) ;
   int size ;
   unsigned int padding ;
};

typedef struct hash *Hash;

struct vert_st {
   int mindist ;
   struct vert_st *next ;
   Hash edgehash ;
   unsigned int padding ;
};

typedef struct vert_st *Vertex;

struct graph_st {
   struct vert_st *__attribute__((array)) vlist ; //JHALA: each cell=0 or validptr into array
};

typedef struct graph_st *Graph;

struct blue_return {
   Vertex vert ;
   int dist ;
};

typedef struct blue_return *BlueReturn;

/******************************************************************/
/********************* Malloc Prototypes **************************/
/******************************************************************/

extern char *malloc(int);
extern Hash malloc_Hash(int) ;
extern HashEntry *__attribute__((array)) malloc_HashEntry_array(int) ;
extern HashEntry malloc_HashEntry(int);
extern Hash MakeHash(int size);

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
  // JHALA MOD/SHIFT return ((int )((key >> 4) % (unsigned int )HashRange));
  int r;
  r = nondet();
  if (0 <= r && r < HashRange) return r;
  L: goto L;     
}

//void HashInsert(void *entry , unsigned int key , Hash hash )  JHALA POLY ISSUE
void HashInsert(int entry , unsigned int key , Hash hash ) 
{ HashEntry ent ;
  int j ;
  /* void *tmp; */
  {
  // assert(3,!HashLookup(key,hash));
  j = /*(*(hash->mapfunc))*/hashfunc(hash->size, key);
  assert(j>=0);
  assert(j<hash->size);
  /*tmp*/ent = /*localmalloc*/malloc_HashEntry((int )sizeof(*ent));
  //ent = (struct hash_entry *)tmp;
  validptr(hash->array + j);
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
    validptr(src);
    hash = src->edgehash;
    i = 0;
    while (i < numvert) {
      if (i != j) {
        dist = compute_dist(i, j, numvert);
        dest = retval->vlist + i;
	validptr(dest);
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
    validptr(vf);
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


int dealwithargs(int argc , string_array argv) 
{ int level ;
  if (argc > 1) {
    validptr(argv + 1);
    level = atoi(*(argv + 1));
  } else {
    level = 1024;
  }
  return (level);
}

int main(int argc, string_array argv ){ 
  Graph graph;
  int dist ;
  int size ;
  {
  size  = dealwithargs(argc, argv);
  graph = MakeGraph(size);
  //dist  = ComputeMst(graph, size);
  exit(0);
}
}

