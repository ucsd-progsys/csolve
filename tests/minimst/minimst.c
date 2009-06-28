extern char *malloc(int);
extern int atoi(const char *);

struct vert_st {
   struct vert_st *next ;
};
typedef struct vert_st *Vertex;

struct graph_st {
   int numvert ;
   Vertex vlist ;
};
typedef struct graph_st *Graph;

void AddEdges(Graph retval, int numvert)
{ Vertex src ;
  Vertex dest ;
  int i ;
  int j ;
  int num_inserted ;

  {
  num_inserted = 0;
  j = 0;
  while (j < numvert) {
    src = retval->vlist + j;
    i = 0;
    while (i < numvert) {
      if (i != j) {
        dest = retval->vlist + i;
        num_inserted ++;
      }
      i ++;
    }
    j ++;
  }
  return;
}
}

Graph MakeGraph(int numvert )
{ int i ;
  Vertex vf ;
  Vertex vt ;
  Graph retval ;
  void *tmp ;
  void *tmp___0 ;

  {
  tmp = malloc(sizeof(*retval));
  retval = (struct graph_st *)tmp;
  retval->numvert = numvert;
  tmp___0 = malloc((unsigned int )numvert * sizeof(*vf));
  retval->vlist = (struct vert_st *)tmp___0;
  vt = (struct vert_st *)((void *)0);
  i = numvert - 1;

  while (i >= 0) {
      vf = retval->vlist + i;
      vf->next = vt;
      vt = vf;
      i--;
  }

  AddEdges(retval, numvert);
  return retval;
}
}

Vertex BlueRule(Vertex inserted , Vertex vlist )
{ Vertex retval ;
  Vertex tmp ;
  Vertex prev ;
  int count ;
  Vertex next ;

  {
      if (vlist == (Vertex) 0) {
          return ((struct vert_st *)((void *)0));
      }
  prev = vlist;
  retval = vlist;
  count = 0;
  tmp = vlist->next;
  while (tmp) {
    count ++;
    if ((unsigned int )tmp == (unsigned int )inserted) {
      next = tmp->next;
      prev->next = next;
    }
    retval = tmp;
    prev = tmp;
    tmp = tmp->next;
  }
  return (retval);
}
}

void ComputeMst(Graph graph , int numvert )
{ Vertex inserted ;
  Vertex tmp ;
  Vertex MyVertexList ;

  {
  MyVertexList = (Vertex )((void *)0);
  inserted = graph->vlist;
  tmp = inserted->next;
  graph->vlist = (struct vert_st *)((void *)0);
  graph->numvert = 1;
  graph->vlist = tmp;
  MyVertexList = tmp;
  numvert --;
  while (numvert) {
    if ((unsigned int )inserted == (unsigned int )MyVertexList) {
      MyVertexList = MyVertexList->next;
    }
    inserted = BlueRule(inserted, MyVertexList);
    numvert --;
  }
  return;
}
}

int dealwithargs(int argc , char **argv )
{ int level ;

  {
  if (argc > 1) {
    level = atoi((char const   *)*(argv + 1));
  } else {
    level = 1024;
  }
  return (level);
}
}

void main(int argc , char **argv)
{ Graph graph ;
  int size ;

  {
  size = dealwithargs(argc, argv);
  graph = MakeGraph(size);
  ComputeMst(graph, size);
  return;
}
}
