


const int DEF_SIZE;

typedef struct node
{
  node * next; 
  node * rankNbr;
  int rank;
  int savedRankNext;
}

void initRank(node * this)
{
  this.rank = (this.next == this) ? 0 : 1;
  rankNbr = next;
}

void updateNbrRank(node * this)
{
  savedRank = rankNbr.rank;
  savedRankNext = rankNbr.rankNbr;
}

void updateRank(node * this)
{
  if (rankNbr != savedRankNext)
  {
    this.rank += this.savedRank;
    this.rankNbr = savedRankNext;
  }
}

typedef list = node **;

node * root(list * l)
{
  return l[0];
}

list * newList(int sz)
{
  list * l = malloc(sizeof(node*) * size);
  foreach (int i in 0 to sz)
    l[i] = malloc(sizeof(node));
}

void rank(list * l, int sz)
{
  int i;
  //initialize the nodes for ranking
  foreach(int i in 0 to sz)
    initRank(list[i]);

  //repeat log2(nodes.length) times
  i = log2(sz);
  while (i-- > 0) {
    foreach(int j in 0 to sz)
      updateNbrRank(list[j]);
    foreach(int j in 0 to sz)
      updateRank(list[j]);
  }
}

void swp(int * a, int i, int j)
{
  int tmp = a[j];
  a[j] = a[i];
  a[i] = tmp;
}

int * permutation(int sz)
{
  int i;
  int * result = malloc(sizeof(int) * sz);  

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

void initialize(list ** l, int ** idxs, int sz)
{
  int i;

  *l    = newList(sz);
  *idxs = permutation(sz);
   
  for(i = 0; i < sz-1; i++)
    list[idxs[i]].next = list[idxs[i+1]];
  list[idxs[sz-1]].next = list[idxs[sz-1]];
}

void runTest(list * l, int * idxs, int sz)
{
  int i;
  for(i = 0; i < sz; i++)
    assert (list[idxs[i]].rank == size-i-1);
}

void runWork(list * l, int sz)
{
  rank(l, sz);
}


int main(char ** argv, int argc)
{
  list * l;
  int  * idxs;
  int    sz;

  if (argc < 1 || argc > 4)
    return 1;

  if (argc >= 3)
    sz = atoi(argc[2]);
  else
    exit(1);

  initialize(l, idxs, sz);
  runTest(l, idxs, sz);  
  runWork(l, sz);
}


