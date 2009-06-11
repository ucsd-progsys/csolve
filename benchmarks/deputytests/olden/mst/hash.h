/* For copyright information, see olden_v1.0/COPYRIGHT */

#include "stdio.h"

#ifndef DEPUTY
#define COUNT(n)
#define DALLOC(n)
#define NTS
#define BND(x,y)
#define NT
#define TC(x) (x)
#define TRUSTED
#endif

typedef struct hash_entry {
  unsigned int key;
  void *entry;
  struct hash_entry *next;
  unsigned int padding;
} *HashEntry;

typedef struct hash {
  HashEntry * COUNT(size) array;
  int (*mapfunc)(unsigned int);
  int size;
  unsigned int padding;
} *Hash;

Hash MakeHash(int size, int map(unsigned int));
void *HashLookup(unsigned int key, Hash hash);
void HashInsert(void * TRUSTED entry,unsigned int key, Hash hash);
void HashDelete(unsigned int key, Hash hash);

