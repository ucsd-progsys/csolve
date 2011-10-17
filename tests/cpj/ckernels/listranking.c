#include <cpj.h>
#include <stdlib.h>

const int DEF_SIZE;

typedef struct node
{ 
  struct node * next; 
  struct node * rankNbr;
  int rank;
  int savedRank;
  int savedRankNext;
} node;

void initRank(node * this)
{
  this -> rank = (this -> next == this) ? 0 : 1;
  this -> rankNbr = this -> next;
}

void updateNbrRank(node * this)
{
  this -> savedRank = this -> rank;
  this -> savedRankNext = this -> rankNbr;
}

void updateRank(node * this)
{
  if (this -> rankNbr != this -> savedRankNext)
  {
    this -> rank += this -> savedRank;
    this -> rankNbr = this -> savedRankNext;
  }
}

typedef node ** list;

node * root(list * l)
{
  return l[0];
}

list * newList(int sz)
{
  list * l = UNQ malloc(sizeof(node*) * sz);
  foreach (i, 0, sz)
    l[i] = malloc(sizeof(node));
  endfor

  return l;
}

void rank(list * l, int sz)
{
  //initialize the nodes for ranking
  foreach(i, 0, sz)
    initRank(l[i]);
  endfor

  //repeat log2(nodes.length) times
  int i = log2(sz);
  while (i-- > 0) {
    foreach(j, 0, sz)
      updateNbrRank(l[j]);
    endfor
    foreach(j, 0, sz)
      updateRank(l[j]);
    endfor
  }
}

void swp(int * a, int i, int j)
{
  int tmp = a[j];
  a[j] = a[i];
  a[i] = tmp;
}

//NOTE NONSTANDARD INITIALIZATION
int * permutation(int sz)
{
  int i;
  int * result = UNQ malloc(sizeof(int) * sz);  

  for(i = 0; i < sz; i++)
    result[i] = i;
  for (i = 0; i < sz; i++)
  {
    int j = rnd(10) * sz;
    int k = rnd(10) * sz;
    swp(result, j, k);
  }

  return result; 
}

//NOTE TWO STAGE INITIALIZATION of list
void initialize(list * l, int ** idxs, int sz)
{
  int i;

  list l   = *l    = newList(sz);
  int * idxs = *idxs = permutation(sz);
   
  for(i = 0; i < sz; i++)
    l[idxs[i]] -> next = l[idxs[i+1]];
  l[idxs[sz-1]] -> next = l[idxs[sz-1]];
}

void runTest(list l, int * idxs, int sz)
{
  int i;
  for(i = 0; i < sz; i++)
    lcc_assert (l[idxs[i]] -> rank == sz-i-1);
}

void runWork(list l, int sz)
{
  rank(l, sz);
}


int main(char ** argv, int argc)
{
  list * l;
  int  ** idxs;
  int    sz;

  if (argc < 1 || argc > 4)
    return 1;

  if (argc >= 3)
    sz = atoi(argv[2]);
  else
    exit(1);

  l    = malloc(sizeof(list*));
  idxs = malloc(sizeof(int*));

  initialize(l, idxs, sz);
  runTest(*l, *idxs, sz);  
  runWork(l, sz);
  
  return 0;
}


