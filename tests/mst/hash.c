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
#define HashRange 100

static int remaining = 0;
static char * BND(__this, __auto) temp;

static int hashfunc(unsigned int key)
{
  return ((key>>4) % HashRange);
}

#define localfree(sz)

Hash MakeHash(int size)
{
  Hash retval;

  retval = (Hash) malloc(sizeof(*retval));
  retval->size = size;
  retval->array = (HashEntry *) malloc(size*sizeof(retval->array[0]));
  {
    memset((char* FAT)TC(retval->array), 0, size * sizeof(retval->array[0]));
  }
  // retval->mapfunc = map;
  retval->padding = 0;
  return retval;
}

void *HashLookup(unsigned int key, Hash hash)
{
  int j;
  HashEntry ent;

  j = hashfunc(key);
  assert(1,j>=0);
  assert(2,j<hash->size);

  for (ent = hash->array[j]; ent && ent->key!=key; ent=ent->next) ;

  if (ent) return ent->entry;
  return NULL;
}

void HashInsert(void *entry,unsigned int key,Hash hash)
{
  HashEntry ent;
  int j;

  assert(3,!HashLookup(key,hash));

  j = hashfunc(key);

  ent = (HashEntry) malloc(sizeof(*ent));
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

  j = hashfunc(key);
  for (ent=&(hash->array[j]); (*ent) && (*ent)->key!=key; ent=&((*ent)->next));
  assert(4,*ent);
  tmp = *ent;
  *ent = (*ent)->next;
  localfree(tmp);
}




