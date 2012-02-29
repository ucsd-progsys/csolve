#include <cpj.h>
#include <stdlib.h>

const int DEF_SIZE;

typedef int csolve_ghost; 

extern float REF(&& [V >= 0; V <= 1]) rnd(int seed) OKEXTERN;
extern int log2(int REF(V > 0) n) OKEXTERN;

#define VALIDNODE START VALIDPTR ROOM_FOR(struct node)
#define NODEID    REF((4 * (DEREF([V]): int)) = (VVADDR - BLOCK_BEGIN([VVADDR])))
#define NODENX    REF(&& [(DEREF([DEREF([V + 4]): ptr]): int) >= 0; (DEREF([DEREF([V + 4]): ptr]): int) < sz])
#define GOODNODE  NODEID VALIDNODE
#define NNVALIDNODE NNSTART NNVALIDPTR NNROOM_FOR(struct node)

typedef struct node
{ 
  csolve_ghost FINAL id; 
  struct node * VALIDNODE   next;
  struct node * NNVALIDNODE rankNbr;
  int rank;
  int savedRank;
  struct node * NNVALIDNODE savedRankNext;
};

void initRank(struct node * GOODNODE this, int i)
{
  this -> rank = (this -> next == this) ? 0 : 1;
  this -> rankNbr = this -> next;
}

void updateNbrRank(struct node * GOODNODE this, int i)
{
  this -> savedRank = this -> rank;
  this -> savedRankNext = this -> rankNbr;
}
  
void updateRank(struct node * GOODNODE this, int i)
{
  if (this -> rankNbr != this -> savedRankNext)
  {
    this -> rank += this -> savedRank;
    this -> rankNbr = this -> savedRankNext;
  }
}

////typedef node ** ARRAY list;
//node * VALIDNODE LOC(L) 
//root(node *LOC(L) VALIDNODE
//          *ARRAY START VALIDPTR ROOM_FOR(struct node *) l) CHECK_TYPE
//{
//  return l[0];
//}
// 
//node* NNVALIDNODE * ARRAY NNSTART NNVALIDPTR SIZE_GE(4*sz)
//  newList(int REF(V > 0) sz) CHECK_TYPE
//{
//  node** l = /* UNQ */ malloc(sizeof(*l) * sz);
//  //  foreach (i, 0, sz)
//  for (int i = 0; i < sz; i++) {
//    l[i] = malloc(sizeof(**l));
//  }
//  //  endfor
//
//  return l;
//}

extern
  struct node * GOODNODE * VALIDPTR START SIZE_GE(4*sz) ARRAY newList(int REF(V > 0) sz) 
  OKEXTERN;
 
void rank(struct node * GOODNODE * ARRAY START VALIDPTR SIZE_GE(4*sz) l,
          int REF(V > 0) sz)
{
  //initialize the nodes for ranking
  foreach(i, 0, sz)
    initRank(l[i], i);
  endfor

  //repeat log2(nodes.length) times
  int i = log2(sz);
  while (i-- > 0) {
    foreach(j, 0, sz)
      updateNbrRank(l[j], j);
    endfor
    foreach(j, 0, sz)
      updateRank(l[j], j);
    endfor
  }
}

///* void swp(int * ARRAY START VALIDPTR SIZE_GE(4*(i+1)) SIZE_GE(4*(j+1)) a,  */
///*     int REF(V >= 0) i, int REF(V >= 0) j) */
///* void swp(int * ARRAY a, int i, int j) CHECK_TYPE */
///* { */
///*   int tmp = a[j]; */
///*   a[j] = a[i]; */
///*   a[i] = tmp; */
///* } */

int REF(&& [V >= 0; V < sz]) * ARRAY START VALIDPTR SIZE_GE(4*sz) permutation(int REF(V > 0) sz)
{
  int i;
  int tmp;
  int * result = malloc(sizeof(int) * sz);  

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
 
struct node * GOODNODE
            * START VALIDPTR SIZE_GE(4*sz) ARRAY
              initialize(int REF(V > 0) sz)
{
  int i,j,k;

  struct node **l    = newList(sz);
  int          *idxs = permutation(sz);

  for(i = 0; i < sz - 1; i++)
  {
    if(l[j = idxs[i]] && l[k = idxs[i+1]])
      l[j] -> next = l[k];
  }

  if(l[j = idxs[sz-1]])
    l[j] -> next = l[j];

  return l;
}

///* void runTest(node ** ARRAY l, int * ARRAY idxs, int sz) */
///* { */
///*   /\* int i; *\/ */
///*   /\* for(i = 0; i < sz; i++) *\/ */
///*   /\*   csolve_assert (l[idxs[i]] -> rank == sz-i-1); *\/ */
///* } */

 
void runWork(struct node * GOODNODE * ARRAY START VALIDPTR SIZE_GE(4*sz) l, 
             int REF(V > 0) sz)
{
  if(l) {
    rank(l, sz);
  }
}

int main(char ** argv, int argc)
{
  int sz;
  struct node   *l;

  if (argc < 1 || argc > 4)
    return 1;

  if (argc >= 3)
    sz = nondetpos(); //atoi(argv[2]);
  else
    exit(1);

  if (sz <= 0) return 0;

  l    = initialize(sz);
  //runTest(*l, *idxs, sz);  
  runWork(l, sz);
  
  return 0;
}
