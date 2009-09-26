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


/******************************************************************/
/********************* Malloc Prototypes **************************/
/******************************************************************/
extern char *malloc(int);
Hash malloc_Hash(int) ;
HashEntry *__attribute__((array)) malloc_HashEntry_array(int) ;
HashEntry malloc_HashEntry(int);

/******************************************************************/
/****************************** Code ******************************/
/******************************************************************/

Hash MakeHash(int size /* , int (*map)(unsigned int  ) */ ) 
{ Hash retval ;
  void *tmp ;
  void *tmp___0 ;
  {
    tmp = /* localmalloc */malloc((int )sizeof(*retval));
//  retval = (struct hash *)tmp;
//  retval->size = size;
//  tmp___0 = /*localmalloc*/malloc((int )((unsigned int )size * sizeof(*(retval->array + 0))));
//  retval->array = (HashEntry *)tmp___0;
//  //NUKE memset((char *)retval->array, 0, (unsigned int )size * sizeof(*(retval->array + 0)));
//  /* retval->mapfunc = map; */
//  retval->padding = 0U;
    return (retval);
  }
}

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

