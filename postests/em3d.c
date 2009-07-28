extern char *malloc(int);
extern int nondet();
extern int nondetnn();
extern void exit(int);

struct node_t {
   int value ;
   struct node_t *next ;
   int from_count ;
   struct node_t **to_nodes ;
   struct node_t **from_nodes ;
   int *coeffs ;
};
typedef struct node_t node_t;

struct graph_t {
   node_t *e_nodes ;
   node_t *h_nodes ;
};
typedef struct graph_t graph_t;

int gen_number(int range )
{
    int ndnn = nondetnn();

    if (ndnn < range)
        return ndnn;

    return 0;
}

node_t **make_table(int size )
{ node_t **retval ;
  void *tmp ;

  {
      tmp = malloc(size * sizeof(node_t *));
  retval = (node_t **)tmp;
  if (retval == 0) {
      // printf((char const   * __restrict  )"Assertion failure\n");
    exit(-1);
  }

  return (retval);
}
}

void fill_table(int size, node_t **table)
{ int i ;
  void *tmp ;
  int tmp___0 ;
  node_t *ttmp;

  {
  i = 0;
  while (i < size) {
      tmp = malloc(sizeof(node_t ));
    validptr(table + i);
    *(table + i) = (node_t *)tmp;
    tmp___0 = nondet();
    (*(table + i))->value = (int )tmp___0;
    (*(table + i))->from_count = 0;
    if (i > 0) {
        ttmp = *(table + i);
        validptr(table + i - 1);
        (*(table + (i - 1)))->next = ttmp;
    }
    i ++;
  }
  validptr(table + size - 1);
  (*(table + (size - 1)))->next = (struct node_t *)((void *)0);
  return;
}
}

void fill_from_fields(node_t *nodelist , int degree )
{ node_t *cur_node ;
  int j ;
  node_t *other_node ;

  cur_node = nodelist;
  while (cur_node) {
    j = 0;
    while (j < degree) {
        // pmr: For to_nodes, we only know
        //   to_nodes = 0 || (0 < to_nodes && to_nodes = BLOCK_BEGIN(to_nodes) && BLOCK_END(to_nodes) = BLOCK_BEGIN(to_nodes) + 333)
        // But this does does not check that to_nodes is not null
        // validptr(cur_node->to_nodes + j);
      other_node = *(cur_node->to_nodes + j);
      // pmr: Surrender to dynamic checking here --- this requires serious smarts!
      // pmr: validptr(other_node->from_nodes + other_node->from_count);
      *(other_node->from_nodes + other_node->from_count) = cur_node;
      (other_node->from_count) ++;
      j ++;
    }
    cur_node = cur_node->next;
  }
  return;
}

void make_neighbors(node_t *nodelist , int tablesz , node_t **table , int degree )
{ node_t *cur_node ;
  node_t *other_node ;
  int j ;
  int k ;
  node_t **tmp ;
  int tmp___0 ;

  cur_node = nodelist;
  while (cur_node) {
      tmp = malloc(degree * sizeof(node_t *));
    cur_node->to_nodes = (node_t **)tmp;
    j = 0;
    while (j < degree) {
      while (1) {
        tmp___0 = gen_number(tablesz);
        validptr(table + tmp___0);
        other_node = *(table + tmp___0);
        k = 0;
        while (k < j) {
          // pmr: annoyance, should be cur_node->to_nodes
          validptr (tmp + k);
          if ((unsigned int )other_node == (unsigned int )*(cur_node->to_nodes + k)) {
            break;
          }
          k ++;
        }
        if ((k >= j)) {
          break;
        }
      }
      // pmr: annoyance, should be cur_node->to_nodes
      validptr(tmp + j);
      *(cur_node->to_nodes + j) = other_node;
      (other_node->from_count) ++;
      j ++;
    }
    cur_node = cur_node->next;
  }
  return;
}

void update_from_coeffs(node_t *nodelist)
{ node_t *cur_node ;
  int from_count ;
  int k ;
  void *tmp ;
  int *tmp___0 ;
  int tmp___1 ;

  cur_node = nodelist;
  while (cur_node) {
      // pmr: manually expanded
      from_count = cur_node->from_count;
    // pmr: requires modified malloc signature

      tmp = malloc(from_count * sizeof(node_t *));
    cur_node->from_nodes = (node_t **)tmp;
    tmp___0 = malloc(from_count * sizeof(node_t *));
    cur_node->coeffs = (int *)tmp___0;
    k = 0;
    while (k < from_count) {
        tmp___1 = nondet();
        // pmr: annoyance of the same type
        validptr(tmp___0 + k);
      *(cur_node->coeffs + k) = (int )tmp___1;
      k ++;
    }
    cur_node->from_count = 0;
    cur_node = cur_node->next;
  }
  return;
}

graph_t *initialize_graph(void)
{ int num_h_nodes ;
  int num_e_nodes ;
  node_t **h_table ;
  node_t **e_table ;
  graph_t *retval ;
  node_t *el;
  node_t *hl;

  retval = (graph_t *)malloc(sizeof(graph_t));

  num_h_nodes = 666;
  num_e_nodes = 666;
  h_table = make_table(num_h_nodes);
  fill_table(666, h_table);
  e_table = make_table(num_e_nodes);
  fill_table(666, e_table);
  hl = *(h_table + 0);
  make_neighbors(hl, 666, e_table, 333);
  el = *(e_table + 0);
  make_neighbors(el, 666, h_table, 333);
  update_from_coeffs(hl);
  update_from_coeffs(el);
  fill_from_fields(hl, 333);
  fill_from_fields(el, 333);
  retval->e_nodes = el;
  retval->h_nodes = hl;
  return (retval);
}

void compute_nodes(node_t *nodelist )
{ int i ;
  node_t *other_node ;
  int coeff ;
  int value ;

  while (nodelist) {
    i = 0;
    while (i < nodelist->from_count) {
        // pmr: another case where dynamic checking seems inevitable?
        // validptr(nodelist->from_nodes + i);
      other_node = *(nodelist->from_nodes + i);
      // validptr(nodelist->coeffs + i);
      coeff = *(nodelist->coeffs + i);
      value = other_node->value;
      nodelist->value = (int )(nodelist->value - coeff * value);
      i ++;
    }
    nodelist = nodelist->next;
  }
  return;
}

void main()
{ int i ;
  graph_t *graph ;

  graph = initialize_graph();
  graph = graph;

  // print_graph(graph);
  i = 0;
  while (i < 100) {
    compute_nodes(graph->e_nodes);
    compute_nodes(graph->h_nodes);
    // fprintf((FILE * __restrict  )stderr, (char const   * __restrict  )"Completed a computation phase: %d\n",
    //        i);
    // print_graph(graph);
    i ++;
  }
}
