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
  struct node * VALIDNODE FINAL next;
  struct node * NNVALIDNODE rankNbr;
  int rank;
  int savedRank;
  struct node * NNVALIDNODE savedRankNext;
};

//void initRank(struct node * VALIDNODE NONNULL this)
//{
//  this -> rank = (this -> next == this) ? 0 : 1;
//  this -> rankNbr = this -> next;
//}

//void updateNbrRank(node * VALIDNODE this)
//{
//  this -> savedRank = this -> rank;
//  this -> savedRankNext = this -> rankNbr;
//}
//  
//void updateRank(node * VALIDNODE this)
//{
//  if (this -> rankNbr != this -> savedRankNext)
//  {
//    this -> rank += this -> savedRank;
//    this -> rankNbr = this -> savedRankNext;
//  }
//}

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
 
void rank(struct node * GOODNODE * ARRAY START VALIDPTR SIZE_GE(4*sz) l,
          int REF(V > 0) sz)
  CHECK_TYPE
{
  //initialize the nodes for ranking
  foreach(i, 0, sz)
  //for(int i = 0; i < sz; i++) {
    //initRank(l[i]);
    struct node * n = l[i];
    int b = n -> id;
    n -> rank = 5;
  //}
  endfor

  //repeat log2(nodes.length) times
//  int i = log2(sz);
//  while (i-- > 0) {
//    /* foreach(j, 0, sz) */
//  for(int j = 0; j < sz; j++)
//      updateNbrRank(l[j]);
//    /* endfor */
//    /* foreach(j, 0, sz) */
//  for(int j = 0; j < sz; j++)
//      updateRank(l[j]);
//    /* endfor */
//  }
}

///* void swp(int * ARRAY START VALIDPTR SIZE_GE(4*(i+1)) SIZE_GE(4*(j+1)) a,  */
///*     int REF(V >= 0) i, int REF(V >= 0) j) */
///* void swp(int * ARRAY a, int i, int j) CHECK_TYPE */
///* { */
///*   int tmp = a[j]; */
///*   a[j] = a[i]; */
///*   a[i] = tmp; */
///* } */
//
////NOTE NONSTANDARD INITIALIZATION
// int REF(&& [V >= 0; V < sz]) * ARRAY START VALIDPTR SIZE_GE(4*sz) permutation(int REF(V > 0) sz) CHECK_TYPE
//{
//  int i;
//  int tmp;
//  int * result = /* UNQ  */malloc(sizeof(int) * sz);  
//
//  for(i = 0; i < sz; i++)
//    result[i] = i;
//  for (i = 0; i < sz; i++)
//  {
//    int j = rnd(10) * sz;
//    int k = rnd(10) * sz;
//    CSOLVE_ASSUME(j < sz && j >= 0);
//    CSOLVE_ASSUME(k < sz && k >= 0);
//    tmp = result[k];
//    result[k] = result[j];
//    result[j] = tmp;
//    /* swp(result, j, k); */
//  }
//
//  return result; 
//}
 
//NOTE TWO STAGE INITIALIZATION of list
/* void initialize(node * NNVALIDNODE */
/*                      * ARRAY NNSTART NNVALIDPTR SIZE_GE(4*sz) */
/*                      * l, */
/*                 int REF(&& [V >= 0; V < sz]) */
/*                     * NNSTART NNVALIDPTR SIZE_GE(4*sz) ARRAY */
/*                     * idxs, */
/*                 int REF(V > 0) sz) CHECK_TYPE */
/* { */
/*   int i,j,k; */

/*   node ** lst   = *l    = newList(sz); */
/*   int *idxslst = *idxs = permutation(sz); */
   
/*   for(i = 0; i < sz - 1; i++) */
/*   { */
/*     if(lst[j = idxslst[i]] && lst[k = idxslst[i+1]])  */
/*       lst[j] -> next = lst[k]; */
/*   } */
  
/*   if(lst[j = idxslst[sz-1]]) */
/*     lst[j] -> next = lst[j]; */
/* } */

// extern
//   void initialize(node * NNVALIDNODE
//    * ARRAY NNSTART NNVALIDPTR SIZE_GE(4*sz) l,
//    int //REF(&& [V >= 0; V < sz])
//    * ARRAY //NNSTART NNVALIDPTR SIZE_GE(4*sz) idxs,
//    idxs,
//    int REF(V > 0) sz) OKEXTERN;

////AND THIS
//extern
//  int REF(&& [V >= 0; V < sz]) * START SIZE_GE(4*sz) VALIDPTR ARRAY
//      initialize_idxs(int REF(V > 0) sz) OKEXTERN;

//extern
//  struct node * GOODNODE
//  * START VALIDPTR SIZE_GE(4*sz) ARRAY 
////  for some reason i can't get the right type out of initialize_idxs
////      initialize_list(int REF(V >= 0) REF(V < sz) * START SIZE_GE(4*sz) VALIDPTR ARRAY idxs, int REF(V > 0) sz) OKEXTERN;
//      initialize_list(int REF(V > 0) sz) OKEXTERN;
//
//
//
///* void runTest(node ** ARRAY l, int * ARRAY idxs, int sz) */
///* { */
///*   /\* int i; *\/ */
///*   /\* for(i = 0; i < sz; i++) *\/ */
///*   /\*   csolve_assert (l[idxs[i]] -> rank == sz-i-1); *\/ */
///* } */
//
// 
//void runWork(struct node * GOODNODE * ARRAY START VALIDPTR SIZE_GE(4*sz) l, 
//             int REF(V > 0) sz)
//{
//  rank(l, sz);
//}
//
//
///* MK NOTES
// * 1) how much of initialization can we _not_ extern?
// *     -quite a bit actually
// * 2) can we do without the ghost field? (ie, is the array index always in scope?)
// *     -only if we have another way of attaching the array index to the node ptr
// * */
// 
//int main(char ** argv, int argc)
//  CHECK_TYPE
//{
//  int sz;
//  struct node   *l;
////  int        *idxs;
////  int arrr;
//
//   if (argc < 1 || argc > 4)
//     return 1;
//
//  if (argc >= 3)
//    sz = nondetpos(); //atoi(argv[2]);
//  else
//    exit(1);
//
//  if (sz <= 0) return 0;
//
//  //LOOK AT THIS PAT
////  idxs = initialize_idxs(sz);
////  arrr = idxs[0];
//// csolve_assert(arrr < sz);
//
////  l    = initialize_list(idxs, sz);
//  l    = initialize_list(sz);
//  //runTest(*l, *idxs, sz);  
//  runWork(l, sz);
//  
//  return 0;
//}
//
