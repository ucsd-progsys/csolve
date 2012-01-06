#include <csolve.h>

/*
 1. Inline Globals
 */
/*************************************************************************/
/******************************** TYPEDEFS *******************************/
/*************************************************************************/

struct _Vertices;

struct _Edges {
   int weight ;
   struct _Vertices *source ;
   struct _Vertices *vertex ;
   struct _Edges *next ;
};

typedef struct _Edges Edges;

struct _Vertices {
   int id ;
   Edges * LOC(EL) edges ;
   struct _Vertices *next ;
   int key ;
   Edges * LOC(EL) chosenEdge ;
};

typedef struct _Vertices Vertices;

typedef Vertices Item;

struct _Heap {
   Item * LOC(IL) item ;
   struct _Heap *parent ;
   struct _Heap *child ;
   struct _Heap *forward ;
   struct _Heap *backward ;
   int rank ;
   short marked ;
};

typedef struct _Heap INST(IL, IL) HeapP;

/*************************************************************************/
/******************************* PROTOTYPES ******************************/
/*************************************************************************/

extern void dummyassert(int) OKEXTERN;
extern void REF(0 = 1) exit(int exitValue ) OKEXTERN;
extern int printf (__const char *__restrict __format, ...) OKEXTERN;
extern int atoi (__const char * ARRAY VALIDPTR __nptr)
     OKEXTERN;
void srandom(unsigned int x ) OKEXTERN;

Vertices *GenGraph(int nVertex , int nEdge ) ;

void PrintGraph(Vertices *graph ) ;

typedef HeapP * LOC(HL) * ARRAY HeapP_array;

//void InitFHeap(void) ;
HeapP_array InitFHeap(void);

HeapP *MakeHeap(HeapP_array hTable) ;

Item *FindMin(HeapP *h, HeapP_array hTable) ;

HeapP *Insert(HeapP **h , Item *i , HeapP_array hTable) ;

HeapP *DeleteMin(HeapP *h , HeapP_array hTable) ;

void PrintMST(Vertices *graph ) ;

Vertices *MST(Vertices *graph ) ;

extern void * LOC(!L) START NONNULL SIZE(__size)
     malloc (int REF(V >= 0) IGNORE_INDEX __size) OKEXTERN;

extern int fprintf(int * , char * __attribute__((__array__))   , ...) ;

extern long int random (void) OKEXTERN;

Vertices *GenTree(int nVertex ) ;

Vertices *AddEdges(Vertices *graph , int nVertex , int nEdge ) ;

Vertices *PickVertex(Vertices *graph , int whichVertex ) ;

void Connect(Vertices *vertex1 , Vertices *vertex2 ) ;

int Duplicate(Vertices *vertex1 , Vertices *vertex2 ) ;

Vertices *NewVertex(void) ;

Edges *NewEdge(void) ;

void PrintNeighbors(Vertices *vertex ) ;
int LessThan(Item * LOC(L) item1 , Item * LOC(L) item2 ) ;

/* int Equal(Item * LOC(L) item1 , Item * LOC(L) item2 ) ; */

Item * LOC(L) Subtract(Item * LOC(L) item , int delta ) ;

HeapP *Meld(HeapP *h1 , HeapP *h2 , HeapP_array hTable) ;

HeapP *DecreaseKey(HeapP *h , HeapP *i , int delta , HeapP_array hTable) ;

HeapP *Delete(HeapP *h , HeapP *i , HeapP_array hTable) ;

HeapP *Find(HeapP *h , Item *item , HeapP_array hTable) ;

Item *ItemOf(HeapP *h , HeapP_array hTable) ;

void CombineLists(HeapP *h1 , HeapP *h2 , HeapP_array hTable) ;

void AddEntry(HeapP *h1 , HeapP *h2 , HeapP_array hTable) ;

HeapP *RemoveEntry(HeapP *h , HeapP_array hTable) ;

HeapP *NewHeap(Item *i , HeapP_array hTable) ;

void RemoveChild(HeapP *i , HeapP_array hTable) ;

void FixRank(HeapP *h , int delta , HeapP_array hTable) ;


/*************************************************************************/
/******************************** GLOBALS ********************************/
/*************************************************************************/

// static int rand_type  =    3;  //JHALA
// static int id  =    1;	  //JHALA
//static HeapP *hTable[10000]  ;  //JHALA

/*************************************************************************/
/******************************** CODE ***********************************/
/*************************************************************************/


int
main (int argc, char * ARRAY VALIDPTR * START NONNULL ARRAY SIZE(argc * 4) argv)
  CHECK_TYPE
{ int nVertex ;
  int nEdge ;
  Vertices *graph ;
  int tmp ;

  int debug  =    1;//JHALA
  {
  nVertex = 10;
  nEdge = 9;
  if (argc > 1) {
    nVertex = atoi((char const   *)*(argv + 1));
    if (argc > 2) {
      nEdge = atoi((char const   *)*(argv + 2));
      if (argc > 3) {
	tmp = atoi((char const   *)*(argv + 3));
        srandom((unsigned int )tmp);
      }
    }
  }
  if (debug) {
    //printf((char * __attribute__((__array__)) )"Generating a connected graph ... ");
  }
  CSOLVE_ASSUME (nVertex > 0); // pmr: contract
  graph = GenGraph(nVertex, nEdge);
  if (debug) {
    //printf((char * __attribute__((__array__)) )"done\nFinding the mininmum spanning tree ... ");
  }
  graph = MST(graph);
  if (debug) {
    //printf((char * __attribute__((__array__)) )"done\nThe graph:\n");
    PrintGraph(graph);
    //printf((char * __attribute__((__array__)) )"The minimum spanning tree:\n");
    PrintMST(graph);
  }
  if (debug) {
    //printf((char * __attribute__((__array__)) )"Time spent in finding the mininum spanning tree:\n");
  }
  exit(0);
  return (0);
}
}

Vertices * LOC(L) MST(Vertices * LOC(L) graph ) 
{ HeapP *heap ;
  Vertices *vertex ;
  Edges *edge ;

  {
  HeapP_array hTable = InitFHeap(); //JHALA
  vertex = graph;
  vertex->key = 0;
  heap = MakeHeap(hTable); 	 //JHALA
  Insert(&heap, vertex, hTable); //JHALA
  vertex = vertex->next;
  while ((unsigned int )vertex != (unsigned int )graph) {
    vertex->key = 2147483647;
    vertex = vertex->next;
  }
  while ((unsigned int )vertex != (unsigned int )graph) {

  }
  vertex = FindMin(heap, hTable); //JHALA
  while ((unsigned int )vertex != (unsigned int )((void *)0)) {
    heap = DeleteMin(heap, hTable); //JHALA
    vertex->key = (-0x7FFFFFFF-1);
    edge = vertex->edges;
    while ((unsigned int )edge != (unsigned int )((void *)0)) {
      struct _Vertices *edgevertex = edge->vertex; // pmr: constprop
      CSOLVE_ASSUME (edgevertex != 0);            // pmr: delayed init
      if (edge->weight < (edgevertex)->key) {
	(edgevertex)->key = edge->weight;
        (edgevertex)->chosenEdge = edge;
        Insert(& heap, edgevertex, hTable); //JHALA
      }
      edge = edge->next;
    }
    vertex = FindMin(heap, hTable); //JHALA
  }
  return (graph);
}
}

void PrintMST(Vertices *graph ) 
{ Vertices *vertex ;

  {
  csolve_assert((unsigned int )graph != (unsigned int )((void *)0));
  vertex = graph->next;
  while ((unsigned int )vertex != (unsigned int )graph) {
    //printf((char * __attribute__((__array__)) )"vertex %d to %d\n", vertex->id, ((vertex->chosenEdge)->source)->id);
    vertex = vertex->next;
  }
  return;
}
}

Vertices *GenGraph(int nVertex , int nEdge ) 
{ Vertices *graph ;

  {
  int generatedEdges  ;
  CSOLVE_ASSUME(nEdge + 1 >= nVertex); 			//JHALA: contract
  CSOLVE_ASSUME(nEdge <= (nVertex * (nVertex - 1)) / 2);  	//JHALA: contract
  generatedEdges = 0;
  graph = GenTree(nVertex);
  graph = AddEdges(graph, nVertex, (nEdge - nVertex) + 1);
  return (graph);
}
}

Vertices *GenTree(int nVertex ) 
{ int i ;
  int weight ;
  Vertices *vertex ;
  Vertices *graph ;
  Edges *edge ;
  long tmp ;
  long tmp___0 ;

  {
  graph = NewVertex();
  graph->next = graph;
  i = 1;
  while (i < nVertex) {
    vertex = NewVertex();
    edge = NewEdge();
    vertex->edges = edge;
    tmp = random();
    edge->vertex = PickVertex(graph, (int )(tmp % (long )i));
    tmp___0 = random();
    weight = (int )((tmp___0 + 1L) % 100L);
    edge->weight = weight;
    edge->source = vertex;
    vertex->next = graph->next;
    graph->next = vertex;
    edge = NewEdge();
    edge->weight = weight;
    Edges *vertexedges = vertex->edges; 			//JHALA constprop
    CSOLVE_ASSUME(vertexedges != (Edges *) 0);			//JHALA delayed-init
    edge->source = (vertexedges)->vertex;		   	//JHALA constprop
    edge->vertex = vertex;
    Vertices *vertexedgesvertex = (vertex->edges)->vertex; 	//JHALA constprop
    CSOLVE_ASSUME(vertexedgesvertex != (Vertices *) 0);		//JHALA delayed-init
    edge->next = (vertexedgesvertex)->edges;			//JHALA constprop
    (vertexedgesvertex)->edges = edge;				//JHALA constprop
    i ++;
  }
  return (graph);
}
}

Vertices * LOC(L) AddEdges(Vertices * LOC(L) graph , int nVertex , int nEdge ) 
{ int i ;
  Vertices *vertex1 ;
  Vertices *vertex2 ;
  long tmp ;
  long tmp___0 ;
  int tmp___1 ;

  {
  csolve_assert((unsigned int )graph != (unsigned int )((void *)0));
  csolve_assert(nEdge >= 0);
  i = 0;
  while (i < nEdge) {
    while (1) {
      tmp = random();
      vertex1 = PickVertex(graph, (int )(tmp % (long )nVertex));
      tmp___0 = random();
      vertex2 = PickVertex(vertex1->next, (int )(tmp___0 % (long )nVertex - 1L));
      dummyassert((unsigned int )vertex1 != (unsigned int )vertex2);
      tmp___1 = Duplicate(vertex1, vertex2);
      if (! tmp___1) {
        break;
      }
    }
    Connect(vertex1, vertex2);
    i ++;
  }
  return (graph);
}
}

Vertices * LOC(L) PickVertex(Vertices * LOC(L) graph , int whichVertex ) 
{ int i ;

  {
  i = 0;
  while (i < whichVertex) {
    graph = graph->next;
    i ++;
  }
  return (graph);
}
}

void Connect(Vertices * LOC(L) vertex1 , Vertices * LOC(L) vertex2 ) 
{ int weight ;
  Edges *edge ;
  long tmp ;

  {
  tmp = random();
  weight = (int )((tmp + 1L) % 100L);
  edge = NewEdge();
  edge->weight = weight;
  edge->source = vertex1;
  edge->vertex = vertex2;
  edge->next = vertex1->edges;
  vertex1->edges = edge;
  edge = NewEdge();
  edge->weight = weight;
  edge->source = vertex2;
  edge->vertex = vertex1;
  edge->next = vertex2->edges;
  vertex2->edges = edge;
  return;
}
}

int Duplicate(Vertices *vertex1 , Vertices *vertex2 ) 
{ Edges *edge ;

  {
  edge = vertex1->edges;
  while ((unsigned int )edge != (unsigned int )((void *)0)) {
    if ((unsigned int )edge->vertex == (unsigned int )vertex2) {
      return (1);
    }
    edge = edge->next;
  }
  return (0);
}
}

Vertices *NewVertex(void) 
{ Vertices *vertex ;
  void *tmp ;
  int tmp___0 ;

  {
  tmp = malloc(sizeof(Vertices ));
  vertex = (Vertices *)tmp;
  //JHALA init issue, this breaks block, adds bogus-fold
  //if ((unsigned int )vertex == (unsigned int )((Vertices *)0)) {
    //JHALA fprintf((int *)2, (char * __attribute__((__array__)) )"Could not malloc\n");
  //  exit(1);
  //}
  // JHALA tmp___0 = id;
  // JHALA id ++;
  vertex->id = nondetpos() + 1; //JHALA tmp___0;
  vertex->edges = (Edges *)0;
  //vertex->next = (struct _Vertices *)0;
  vertex->next = vertex; //JHALA
  return (vertex);
}
}

Edges *NewEdge(void) 
{ Edges *edge ;
  void *tmp ;

  {
  tmp = malloc(sizeof(Edges ));
  edge = (Edges *)tmp;
  if ((unsigned int )edge == (unsigned int )((Edges *)0)) {
    //JHALA fprintf((int *)2, (char * __attribute__((__array__)) )"Could not malloc\n");
    exit(1);
  }
  edge->weight = 0;
  edge->vertex = (struct _Vertices *)0;
  edge->next = (struct _Edges *)0;
  return (edge);
}
}

void PrintGraph(Vertices *graph ) 
{ Vertices *vertex ;

  {
  csolve_assert((unsigned int )graph != (unsigned int )((Vertices *)0));
  vertex = graph;
  while (1) {
    //printf((char * __attribute__((__array__)) )"Vertex %d is connected to:", vertex->id);
    PrintNeighbors(vertex);
    //printf((char * __attribute__((__array__)) )"\n");
    vertex = vertex->next;
    if (! ((unsigned int )vertex != (unsigned int )graph)) {
      break;
    }
  }
  return;
}
}

void PrintNeighbors(Vertices *vertex ) 
{ Edges *edge ;

  {
  edge = vertex->edges;
  while ((unsigned int )edge != (unsigned int )((Edges *)0)) {
    // printf((char * __attribute__((__array__)) )" %d(%d)[%d]", (edge->vertex)->id, edge->weight, (edge->source)->id);
    edge = edge->next;
  }
  return;
}
}

int LessThan(Item *item1 , Item *item2 ) 
{ 
  {
    return (item1->key < item2->key);
}
}

/* int Equal(Item *item1 , Item *item2 )  */
/* {  */
/*   { */
/*     validptr(item1); */
/*     validptr(item2); */
/*   return (item1->key == item2->key); */
/* } */
/* } */

//Item *Subtract(Item *item , int delta ) 
//{ 
//
//  {
//  // csolve_assert(delta > 0);
//  validptr(item);
//  item->key -= delta;
//  return (item);
//}
//}


HeapP_array InitFHeap(void) 
{ int j ;

  HeapP *hTable[10000]  ;  //JHALA
  {
  j = 0;
  while (j < 10000) {
    hTable[j] = (HeapP *)0;
    j ++;
  }
  return hTable ;
}
}

HeapP *MakeHeap(HeapP_array hTable) 
{ 

  {
  return ((HeapP *)0);
}
}

Item * LOC(IL) FindMin(HeapP INST(IL, IL) * LOC(L) h , HeapP_array INST(HL, L) hTable) 
{ 

  {
  if ((unsigned int )h == (unsigned int )((HeapP *)0)) {
    return ((Item *)0);
  } else {
    return (h->item);
  }
}
}

HeapP INST(IL, IL) * LOC(L) Insert(HeapP * LOC(L) *h, Item * LOC(IL) i , HeapP_array INST(HL, L) hTable) 
{ HeapP *h1 ;

  {
  h1 = NewHeap(i, hTable);
  *h = Meld(*h, h1, hTable);
  return (h1);
}
}

HeapP * LOC(L) Meld(HeapP * LOC(L) h1 , HeapP * LOC(L) h2 , HeapP_array INST(HL, L) hTable) 
{ int tmp ;

  {
  if ((unsigned int )h2 == (unsigned int )((HeapP *)0)) {
    return (h1);
  }
  if ((unsigned int )h1 == (unsigned int )((HeapP *)0)) {
    return (h2);
  }
  CombineLists(h1, h2, hTable);
  tmp = LessThan(h1->item, h2->item);
  if (tmp) {
    return (h1);
  } else {
    return (h2);
  }
}
}

HeapP * LOC(L) DeleteMin(HeapP * LOC(L) h , HeapP_array INST(HL, L) hTable) 
{ int r ;
  int rMax ;
  int j ;
  HeapP *h1 ;
  HeapP *h2 ;
  HeapP *h3 ;
  HeapP *min ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  HeapP *hTr; //JHALA : constprop
  {
  rMax = 0;
  if ((unsigned int )h == (unsigned int )((HeapP *)0)) {
    return ((HeapP *)0);
  }
  h1 = RemoveEntry(h, hTable);
  if ((unsigned int )h1 == (unsigned int )((HeapP *)0)) {
    // free((void *)h);
    return ((HeapP *)0);
  }
  if ((unsigned int )h1 == (unsigned int )h->child) {
    h->child = (struct _Heap *)0;
  }
  h2 = h1;
  while (1) {
    h3 = h2->forward;
    h2->forward = h2;
    h2->backward = h2;
    h2->parent = (struct _Heap *)0;
    r = h2->rank;
    csolve_assert(r < 10000);
    hTr = hTable[r]; 
    while ((unsigned int )hTr != (unsigned int )((HeapP *)0)) { //JHALA: constprop
      tmp = LessThan((hTr)->item, h2->item);			//JHALA: constprop
      if (tmp) {
        AddEntry(hTr, h2, hTable);				//JHALA: constprop
        h2 = hTr;						//JHALA: constprop
      } else {
        AddEntry(h2, hTr, hTable);				//JHALA: constprop
      }
      hTable[r] = (HeapP *)0;
      r = h2->rank;
      csolve_assert(r < 10000);
      hTr = hTable[r];
    }
    hTable[r] = h2;
    if (r > rMax) {
      rMax = r;
    }
    h2 = h3;
    if (! ((unsigned int )h2 != (unsigned int )h1)) {
      break;
    }
  }
  if ((unsigned int )h->child != (unsigned int )((struct _Heap *)0)) {
    h2 = h->child;
    while (1) {
      h3 = h2->forward;
      h2->forward = h2;
      h2->backward = h2;
      h2->parent = (struct _Heap *)0;
      r = h2->rank;
      csolve_assert(r < 10000);

      hTr = hTable[r];						//JHALA: constprop
      while (hTr != ((HeapP *)0)) {				//JHALA: constprop
	tmp___0 = LessThan((hTr)->item, h2->item);		//JHALA: constprop
        if (tmp___0) {
          AddEntry(hTr, h2, hTable);				//JHALA: constprop
          h2 = hTr;						//JHALA: constprop
        } else {
          AddEntry(h2, hTr, hTable);				//JHALA: constprop
        }
        hTable[r] = (HeapP *)0;
        r = h2->rank;
        csolve_assert(r < 10000);
	hTr = hTable[r];
      }

      hTable[r] = h2;
      if (r > rMax) {
        rMax = r;
      }
      h2 = h3;
      if (! ((unsigned int )h2 != (unsigned int )h->child)) {
        break;
      }
    }
  }
  j = 0;
  while (j <= rMax) {
    if ((unsigned int )hTable[j] != (unsigned int )((HeapP *)0)) {
      break;
    }
    j ++;
  }
  //JHALA: seems like a valid overflow in value of j! BUG?
  CSOLVE_ASSUME(j < 10000);
  h1 = hTable[j];
  min = h1;
  CSOLVE_ASSUME(min != (HeapP *) 0);
  hTable[j] = (HeapP *)0;
  j ++;
  while (j <= rMax) {
    HeapP *hTj = hTable[j];					//JHALA: constprop
    if ((unsigned int )hTj != (unsigned int )((HeapP *)0)) {	//JHALA: constprop
      CombineLists(h1, hTj, hTable);				//JHALA: constprop
      tmp___1 = LessThan((hTj)->item, min->item);		//JHALA: constprop
      if (tmp___1) {
        min = hTj;						//JHALA: constprop
      }
      hTable[j] = (HeapP *)0;
    }
    j ++;
  }
  // free((void *)h);
  return (min);
}
}

//HeapP *DecreaseKey(HeapP *h , HeapP *i , int delta , HeapP_array hTable) 
//{ int tmp ;
//
//  {
//  // csolve_assert((unsigned int )h != (unsigned int )((HeapP *)0));
//  // csolve_assert((unsigned int )i != (unsigned int )((HeapP *)0));
//  if (! ((unsigned int )i->parent == (unsigned int )((void *)0))) {
//    RemoveChild(i, hTable);
//    CombineLists(h, i, hTable);
//  }
//  i->item = Subtract(i->item, delta);
//  tmp = LessThan(i->item, h->item);
//  if (tmp) {
//    return (i);
//  } else {
//    return (h);
//  }
//}
//}

//void RemoveChild(HeapP *i , HeapP_array hTable) 
//{ HeapP *parent ;
//
//  {
//  // csolve_assert((unsigned int )i != (unsigned int )((HeapP *)0));
//  parent = i->parent;
//  csolve_assert((unsigned int )parent != (unsigned int )((HeapP *)0));
//  if ((unsigned int )parent->child == (unsigned int )i) {
//    if ((unsigned int )i == (unsigned int )i->forward) {
//      parent->child = (struct _Heap *)0;
//    } else {
//      parent->child = i->forward;
//    }
//  }
//  RemoveEntry(i, hTable);
//  FixRank(parent, i->rank + 1, hTable);
//  i->forward = i;
//  i->backward = i;
//  i->parent = (struct _Heap *)0;
//  return;
//}
//}

//void FixRank(HeapP *h , int delta , HeapP_array hTable) 
//{ 
//
//  {
//  csolve_assert((unsigned int )h != (unsigned int )((HeapP *)0));
//  csolve_assert(delta > 0);
//  while (1) {
//    h->rank -= delta;
//    h = h->parent;
//    if (! ((unsigned int )h != (unsigned int )((HeapP *)0))) {
//      break;
//    }
//  }
//  return;
//}
//}

//HeapP *Delete(HeapP *h , HeapP *i , HeapP_array hTable) 
//{ HeapP *h1 ;
//  HeapP *h2 ;
//  HeapP *tmp ;
//  int tmp___0 ;
//
//  {
//  csolve_assert((unsigned int )h != (unsigned int )((HeapP *)0));
//  csolve_assert((unsigned int )i != (unsigned int )((HeapP *)0));
//  if ((unsigned int )h == (unsigned int )i) {
//    tmp = DeleteMin(h, hTable);
//    return (tmp);
//  }
//  if ((unsigned int )i->parent == (unsigned int )((void *)0)) {
//    RemoveEntry(i, hTable);
//  } else {
//    RemoveChild(i, hTable);
//  }
//  h1 = i->child;
//  if ((unsigned int )h1 != (unsigned int )((HeapP *)0)) {
//    while (1) {
//      h2 = h1->forward;
//      h1->forward = h1;
//      h1->backward = h1;
//      h1->parent = (struct _Heap *)0;
//      CombineLists(h, h1, hTable);
//      tmp___0 = LessThan(h1->item, h->item);
//      if (tmp___0) {
//        h = h1;
//      }
//      h1 = h2;
//      if (! ((unsigned int )h1 != (unsigned int )i->child)) {
//        break;
//      }
//    }
//  }
//  //  free((void *)i);
//  return (h);
//}
//}

void CombineLists(HeapP * LOC(L) h1 , HeapP * LOC(L) h2 , HeapP_array INST(HL, L) hTable) 
{ HeapP *h ;
  int tmp ;

  {
  if ((unsigned int )h1 != (unsigned int )((HeapP *)0)) {
    if ((unsigned int )h2 != (unsigned int )((HeapP *)0)) {
      tmp = 1;
    } else {
      tmp = 0;
    }
  } else {
    tmp = 0;
  }
  csolve_assert(tmp);
  h = h1;
  (h1->forward)->backward = h2;
  (h2->forward)->backward = h1;
  h = h1->forward;
  h1->forward = h2->forward;
  h2->forward = h;
  return;
}
}

void AddEntry(HeapP * LOC(L) h1 , HeapP * LOC(L) h2 , HeapP_array INST(HL, L) hTable) 
{ int tmp ;

  {
  if ((unsigned int )h1 != (unsigned int )((HeapP *)0)) {
    if ((unsigned int )h2 != (unsigned int )((HeapP *)0)) {
      tmp = 1;
    } else {
      tmp = 0;
    }
  } else {
    tmp = 0;
  }
  csolve_assert(tmp);
  if ((unsigned int )h1->child == (unsigned int )((struct _Heap *)0)) {
    h1->child = h2;
  } else {
    CombineLists(h1->child, h2, hTable);
  }
  h2->parent = h1;
  //JHALA
  int rtmp = (h1->rank + h2->rank) + 1;
  CSOLVE_ASSUME(rtmp < 10000);
  h1->rank = rtmp;
  
  return;
}
}

HeapP * LOC(L) RemoveEntry(HeapP * LOC(L) h , HeapP_array INST(HL, L) hTable) 
{ 
  csolve_assert((unsigned int )h != (unsigned int )((HeapP *)0));
  if ((unsigned int )h == (unsigned int )h->forward) {
    return (h->child);
  }
  (h->forward)->backward = h->backward;
  (h->backward)->forward = h->forward;
  return (h->forward);
}

HeapP INST(IL, IL) * LOC(L) NewHeap(Item * LOC(IL) i , HeapP_array INST(HL, L) hTable) 
{ HeapP *h ;
  void *tmp ;
  {
  tmp = malloc(sizeof(HeapP ));
  h = (HeapP *)tmp;
  if ((unsigned int )h == (unsigned int )((HeapP *)0)) {
    //JHALA fprintf((int *)2, (char * __attribute__((__array__)) )"Oops, could not malloc\n");
    exit(1);
  }
  h->item = i;
  h->parent = (struct _Heap *)0;
  h->child = (struct _Heap *)0;
  h->forward = h;
  h->backward = h;
  h->rank = 0;
  h->marked = (short)0;
  return (h);
}
}

//Item *ItemOf(HeapP *h , HeapP_array hTable) 
//{ 
//  {
//  return (h->item);
//}
//}

//HeapP *Find(HeapP *h , Item *item , HeapP_array hTable) 
//{ HeapP *h1 ;
//  HeapP *h2 ;
//  int tmp ;
//  int tmp___0 ;
//
//  {
//  if ((unsigned int )h == (unsigned int )((HeapP *)0)) {
//    return ((HeapP *)0);
//  }
//  h1 = h;
//  while (1) {
//    tmp___0 = Equal(h1->item, item);
//    if (tmp___0) {
//      return (h1);
//    } else {
//      tmp = LessThan(h1->item, item);
//      if (tmp) {
//        h2 = Find(h1->child, item, hTable);
//        if ((unsigned int )h2 != (unsigned int )((HeapP *)0)) {
//          return (h2);
//        }
//      }
//    }
//    h1 = h1->forward;
//    if (! ((unsigned int )h1 != (unsigned int )h)) {
//      break;
//    }
//  }
//  return ((HeapP *)0);
//}
//}


/*************************************************************************/
/************************** RANDOM ***************************************/
/*************************************************************************/

//static long randtbl[32]  = 
//  {      3L,      -851904987L,      -43806228L,      -2029755270L, 
//        1390239686L,      -1912102820L,      -485608943L,      1969813258L, 
//        -1590463333L,      -1944053249L,      455935928L,      508023712L, 
//        -1714531963L,      1800685987L,      -2015299881L,      654595283L, 
//        -1149023258L,      -1470005550L,      -1143256056L,      -1325577603L, 
//        -1568001885L,      1275120390L,      -607508183L,      -205999574L, 
//        -1696891592L,      1492211999L,      -1528267240L,      -952028296L, 
//        -189082757L,      362343714L,      1424981831L,      2039449641L};
//static long *fptr   = & randtbl[4];
//static long *rptr   = & randtbl[1];
//static long *state  = & randtbl[1];
//static long end_ptr = (long )(& randtbl[sizeof(randtbl) / sizeof(randtbl[0])]);
//
//
//void srandom(unsigned int x ) 
//{ register long i ;
//  
//  static int rand_sep  =    3;  //JHALA
//  static int rand_deg  =    31; //JHALA
//  {
//  *(state + 0) = (long )x;
//  if (/* JHALA rand_type */ 3 != 0) {
//    i = 1L;
//    while (i < (long )rand_deg) {
//      *(state + i) = 1103515145L * *(state + (i - 1L)) + 12345L;
//      i ++;
//    }
//    fptr = state + rand_sep;
//    rptr = state + 0;
//    i = 0L;
//    while (i < (long )(10 * rand_deg)) {
//      random();
//      i ++;
//    }
//  }
//  return;
//}
//}
//
//
//long random(void) 
//{ long i ;
//  {
//  if (/* JHALA rand_type */ 3 == 0) {
//    *(state + 0) = (*(state + 0) * 1103515245L + 12345L) & 2147483647L;
//    return (*(state + 0));
//  } else {
//    *fptr += *rptr;
//    i = (*fptr >> 1) & 2147483647L;
//    fptr ++;
//    if ((unsigned int )fptr >= (unsigned int )end_ptr) {
//      fptr = state;
//      rptr ++;
//    } else {
//      rptr ++;
//      if ((unsigned int )rptr >= (unsigned int )end_ptr) {
//        rptr = state;
//      }
//    }
//    return (i);
//  }
//}
//}
