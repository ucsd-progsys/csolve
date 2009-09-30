/******************************************************************/
/********************* Type Definitions ***************************/
/******************************************************************/

typedef char *__attribute__((array)) *__attribute__((array)) string_array;

struct hash_entry {
   unsigned int key ;
   void *entry ;
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
Hash malloc_Hash(int) ;
HashEntry *__attribute__((array)) malloc_HashEntry_array(int) ;
HashEntry malloc_HashEntry(int);
Hash MakeHash(int size);

/******************************************************************/
/****************************** Code ******************************/
/******************************************************************/

//JHALA: empty store issue
//Hash MakeHash(int size /* , int (*map)(unsigned int  ) */ ) 
//{ Hash retval ;
//  void *tmp ;
//  void *tmp___0 ;
//  {
//    tmp = /* localmalloc */malloc((int )sizeof(*retval));
//    retval = (struct hash *)tmp;
//    retval->size = size;
//    tmp___0 = /*localmalloc*/malloc((int )((unsigned int )size * sizeof(*(retval->array + 0))));
//    retval->array = (HashEntry *)tmp___0;
//    //NUKE memset((char *)retval->array, 0, (unsigned int )size * sizeof(*(retval->array + 0)));
//    /* retval->mapfunc = map; */
//    retval->padding = 0U;
//    return (retval);
//  }
//}

static int hashfunc(/* JHALA: */unsigned int HashRange, unsigned int key ) 
{ 
  return ((int )((key >> 4) % (unsigned int )HashRange));
}

//void HashInsert(void *entry , unsigned int key , Hash hash ) 
//{ HashEntry ent ;
//  int j ;
//  /* void *tmp; */
//  {
//  // assert(3,!HashLookup(key,hash));
//  j = /*(*(hash->mapfunc))*/hashfunc(hash->size, key);
//  assert(j>=0);
//  assert(j<hash->size);
//  /*tmp*/ent = /*localmalloc*/malloc_HashEntry((int )sizeof(*ent));
//  //ent = (struct hash_entry *)tmp;
//  validptr(hash->array + j);
//  
//  ent->next = *(hash->array + j);
//  
//  *(hash->array + j) = ent;
// 
//
//
//
//
//  ent->key = key;
//  
//  ent->entry = entry;
//  
//  return;
//}
//}

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
    i  = nondetpos();
    vf = retval->vlist + i;
    validptr(vf);
    vf->mindist = 9999999;
    vf->edgehash = MakeHash(HashRange/*, & hashfunc*/);
    vf->next = vt;
    vt = vf;
    i --;
  }
  //chatting((char *)"Make phase 3: Creating graph\n");
  //AddEdges(retval, numvert);
  //chatting((char *)"Make returning\n");
  return (retval);
}
}


int dealwithargs(int argc , string_array argv ) 
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
  int dist ;
  int size ;
  {
  size = dealwithargs(argc, argv);
  exit(0);
}
}

