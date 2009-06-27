/* For copyright information, see olden_v1.0/COPYRIGHT */

#include "mst.h"
#include "ssplain.h"

#define CONST_m1 10000
#define CONST_b 31415821
#define RANGE 2048

static void AddEdges(Graph retval, int numvert)
{
  Vertex src, dest;
  int i, j, num_inserted = 0;

  for (j = 0; j < numvert; j++)
    {
      src = &(retval->vlist[j]);

      for (i=0; i<numvert; i++)
        {
          if (i!=j)
            {
              dest = &(retval->vlist[i]);
	      num_inserted++;
            }
        } /* for i */
    } /* for j */

  //  chatting("%d edges inserted\n", num_inserted);
}

Graph MakeGraph(int numvert)
{
  int i;
  Vertex vf, vt;
  Graph retval;

  retval = (Graph) malloc (sizeof(*retval));

  //  chatting("Make phase 1: Creating hash tables\n");
  retval->numvert = numvert;
  retval->vlist = (Vertex) malloc (numvert*(sizeof(*vf)));
  vt = NULL;

  for (i = numvert - 1; i >= 0; i--)
    {
      vf = &(retval->vlist[i]);
      vf->next = vt;
      vt=vf;
    }

  //  chatting("Make phase 3: Creating graph\n");
  AddEdges(retval, numvert);
  //  chatting("Make returning\n");
  return retval;
}
