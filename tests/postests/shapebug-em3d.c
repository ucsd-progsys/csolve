#include <stdlib.h>

// This doesn't seem to care about array attributes

struct node_t {
    struct node_t * __attribute__ ((array)) *__attribute__((array)) to_nodes ;
};
typedef struct node_t node_t;

node_t * __attribute__ ((array)) * __attribute__ ((array)) make_table(int size )
{ node_t **retval ;

  retval = (node_t **)malloc(size * sizeof(node_t *));

  return (retval);
}

void fill_table(int size, node_t * __attribute__ ((array)) * __attribute__ ((array)) table)
{
  return;
}

void initialize_graph(void)
{ node_t **h_table ;

  h_table = make_table(666);
  fill_table(666, h_table);
}
