/* For copyright information, see olden_v1.0/COPYRIGHT */

#include "mst.h"
#include "ssplain.h"

static Vertex BlueRule(Vertex inserted, Vertex vlist)
{
  Vertex retval;
  Vertex tmp, prev;
  int count;

  if (!vlist) {
    return NULL;
  }
  prev = vlist;
  retval = vlist;

  count = 0;

  /* We are guaranteed that inserted is not first in list */
  for (tmp=vlist->next; tmp; prev=tmp,tmp=tmp->next)
    {
      count++;

      /* Splice chosen vertex out of the list */
      if (tmp==inserted)
        {
          Vertex next;
          next = tmp->next;
          prev->next = next;
        }

      retval = tmp;
    } /* for */
  return retval;
}

static void ComputeMst(Graph graph,int numvert)
{
  Vertex inserted,tmp;
  Vertex MyVertexList = NULL;

  /* Insert first node */
  inserted = graph->vlist;
  tmp = inserted->next;
  /* In this second stage of the algorithm, we use the vlist as SAFE nodes */
  // #ifdef DEPUTY
  graph->vlist = NULL;
  graph->numvert = 1;
  // #endif
  graph->vlist = tmp;
  MyVertexList = tmp;
  numvert--;

  /* Announce insertion and find next one */
  while (numvert)
    {
      if (inserted == MyVertexList)
	MyVertexList = MyVertexList->next;
      inserted = BlueRule(inserted, MyVertexList);
      numvert--;
    }
}

extern int dealwithargs(int argc, char *argv[]);

int main(int argc, char *argv[])
{
  Graph graph;
  int size;

  size = dealwithargs(argc, argv);
  graph = MakeGraph(size);
  ComputeMst(graph, size);

  return 0;
}
