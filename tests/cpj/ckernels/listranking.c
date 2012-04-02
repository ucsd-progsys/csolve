#include <cpj.h>
#include <stdlib.h>

const int DEF_SIZE;

extern float REF(&& [V >= 0; V <= 1]) rnd(int seed) OKEXTERN;
extern int log2(int REF(V > 0) n) OKEXTERN;

#define VALIDNODE START VALIDPTR ROOM_FOR(struct node)
#define NNVALIDNODE NNSTART NNVALIDPTR NNROOM_FOR(struct node)

typedef struct node
{ 
  int FINAL id;
  struct node * NNVALIDNODE next; 
  struct node * NNVALIDNODE rankNbr;
  int rank;
  int savedRank;
  struct node * NNVALIDNODE savedRankNext;
} node;

void initRank(node * VALIDNODE this) CHECK_TYPE
{
  this -> rank = (this -> next == this) ? 0 : 1;
  //this -> rankNbr = this -> next;
}

void updateNbrRank(node * VALIDNODE this) CHECK_TYPE
{
  this -> savedRank = this -> rank;
  this -> savedRankNext = this -> rankNbr;
}
  
void updateRank(node * VALIDNODE this) CHECK_TYPE
{
  if (this -> rankNbr != this -> savedRankNext)
  {
    this -> rank += this -> savedRank;
    this -> rankNbr = this -> savedRankNext;
  }
}

/* //typedef node ** ARRAY list; */
/* node * VALIDNODE LOC(L)  */
/* root(node *LOC(L) VALIDNODE */
/*           *ARRAY START VALIDPTR ROOM_FOR(struct node *) l) NO_OP */
/* { */
/*   return l[0]; */
/* } */

node* newNode(int i)
{
  node *n;
  
  n = malloc(sizeof(*n));
  n->id = i;
  
  return n;
}

 
node *VALIDNODE 
     *ARRAY NNSTART NNVALIDPTR NNSIZE_GE(4*sz)
newList(int REF(V > 0) sz)
{
  node** l = /* UNQ */ malloc(sizeof(*l) * sz);
  /* foreach (i, 0, sz) */
  for(int i = 0; i < sz; i++) {
    node *n = malloc(sizeof(*n));
    n->id = i;
    csolve_assert(n->id == i);
    l[i] = n;
  }
  /* endfor */

  return l;
}

void rank(node * NNVALIDNODE * ARRAY START VALIDPTR SIZE_GE(4*sz) l, int REF(V > 0) sz)
{
  //initialize the nodes for ranking
  foreach(i, 0, sz)
    if(l[i])	
      initRank(l[i]);
  endfor

  /* //repeat log2(nodes.length) times */
  /* int i = log2(sz); */
  /* while (i-- > 0) { */
  /*   foreach(j, 0, sz) */
  /*     if(l[j]) */
  /* 	updateNbrRank(l[j]); */
  /*   endfor */
  /*   foreach(j, 0, sz) */
  /*     if(l[j]) */
  /* 	updateRank(l[j]); */
  /*   endfor */
  /* } */
}

/* void swp(int * ARRAY a, int i, int j) */
/* { */
/*   int tmp = a[j]; */
/*   a[j] = a[i]; */
/*   a[i] = tmp; */
/* } */

//NOTE NONSTANDARD INITIALIZATION
 int REF(&& [V >= 0; V < sz]) * ARRAY NNSTART NNVALIDPTR NNSIZE_GE(4*sz) permutation(int REF(V > 0) sz) 
{
  int i;
  int tmp;
  int * result = /* UNQ  */malloc(sizeof(int) * sz);  

  for(i = 0; i < sz; i++)
    result[i] = i;
  for (i = 0; i < sz; i++)
  {
    int j = rnd(10) * sz;
    int k = rnd(10) * sz;
    CSOLVE_ASSUME(j < sz && j >= 0);
    CSOLVE_ASSUME(k < sz && k >= 0);
    tmp = result[k];
    result[k] = result[j];
    result[j] = tmp;
    /* swp(result, j, k); */
  }

  return result; 
}
 
//NOTE TWO STAGE INITIALIZATION of list
void initialize(node * NNVALIDNODE
		* ARRAY NNSTART NNVALIDPTR NNSIZE_GE(4*sz) lst,
                     /* * l, */
                int REF(&& [(V >= 0); (V < sz)])
		* ARRAY NNSTART NNVALIDPTR NNSIZE_GE(4*sz) idxslst,
                    /* * idxs, */
                int REF(V > 0) sz)
{
  int i,j,k;

  /* node ** lst   = *l   = newList(sz); */
  /* int *idxslst = *idxs = permutation(sz); */
   
  for(i = 0; i < sz - 1; i++)
  {
    if(idxslst[i] < sz && idxslst[i+1] < sz) {
      if(lst[j = idxslst[i]] && lst[k = idxslst[i+1]])
      	lst[j] -> next = lst[k];
    }
  }
  
  if((j = idxslst[sz-1]) < sz && lst[j])
    lst[j] -> next = lst[j];
}

/* void runTest(node ** ARRAY l, int * ARRAY idxs, int sz) */
/* { */
/*   /\* int i; *\/ */
/*   /\* for(i = 0; i < sz; i++) *\/ */
/*   /\*   csolve_assert (l[idxs[i]] -> rank == sz-i-1); *\/ */
/* } */

 
void runWork(node * NNVALIDNODE * ARRAY NNSTART NNVALIDPTR NNSIZE_GE(4*sz) l, int REF(V > 0) sz) 
{
  if(l) {
    rank(l, sz);
  }
}
//ABAKST: Fix this
/* int main(char ** argv, int argc)  */
  /* NO_OP */
int main()
{
  int sz;
  node **l;
  int *idxs;

  /* if (argc < 1 || argc > 4) */
  /*   return 1; */

  /* if (argc >= 3) */
  /*   sz = atoi(argv[2]); */
  /* else */
  /*   exit(1); */
  sz = nondetpos();
  if (sz <= 0) return 0;

  l   = newList(sz);
  idxs = permutation(sz);
  
  initialize(l, idxs, sz);
  // runTest(*l, *idxs, sz);
  runWork(l, sz);
  
  return 0;
}
