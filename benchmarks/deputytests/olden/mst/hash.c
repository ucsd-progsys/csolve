/* For copyright information, see olden_v1.0/COPYRIGHT */

#include "ssplain.h"
#ifdef SS_PLAIN
#include "mst.h"
#else //SS_PLAIN
#include "hash.h"
#endif //SS_PLAIN
#include <string.h>

#define assert(num,a) if (!(a)) {chatting("Assertion failure:%d in hash\n",num); exit(-1);}
#undef assert
#define assert(num,a)

static void * (DALLOC(size) localmalloc)(int size);

static int remaining = 0;
static char * BND(__this, __auto) temp;

static void *localmalloc(int size) {
  char *blah;
  
  if (size>remaining) 
    {
      temp = (char *) malloc(32768);
      if (!temp) chatting("Error! malloc returns null\n");
      remaining = 32768;
    }
  blah = temp;
  temp += size;
  remaining -= size;
  return blah;
}
 
#define localfree(sz)

Hash MakeHash(int size, int (*map)(unsigned int)) 
{
  Hash retval;

  retval = (Hash) localmalloc(sizeof(*retval));
  retval->size = size;
  retval->array = (HashEntry *) localmalloc(size*sizeof(retval->array[0]));
  { 
    memset((char* FAT)TC(retval->array), 0, size * sizeof(retval->array[0]));
  }
  retval->mapfunc = map;
  retval->padding = 0; 
  return retval;
}

void *HashLookup(unsigned int key, Hash hash)
{
  int j;
  HashEntry ent;

  j = (hash->mapfunc)(key);
  assert(1,j>=0);
  assert(2,j<hash->size);

  for (ent = hash->array[j]; ent && ent->key!=key; ent=ent->next) ;
  /* This is:
  ent = hash->array[j];
  while (ent && ent->key!=key) ent=ent->next;
  */


  if (ent) return ent->entry;
  return NULL;
}

void HashInsert(void *entry,unsigned int key,Hash hash) 
{
  HashEntry ent;
  int j;
  
  assert(3,!HashLookup(key,hash));
  
  j = (hash->mapfunc)(key);

  ent = (HashEntry) localmalloc(sizeof(*ent));
  ent->next = hash->array[j];
  hash->array[j]=ent;
  ent->key = key;
  ent->entry = entry;
}

void HashDelete(unsigned int key,Hash hash)
{
  HashEntry *ent;
  HashEntry tmp;
  int j;

  j = (hash->mapfunc)(key);
  for (ent=&(hash->array[j]); (*ent) && (*ent)->key!=key; ent=&((*ent)->next));
  assert(4,*ent); //RJ: else the element is not in the list
  tmp = *ent;
  *ent = (*ent)->next;
  localfree(tmp);
}




