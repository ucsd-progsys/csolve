/* em3d.h - Header file for the electromagnetic problem in 3 dimensions
 *
 * By:  Martin C. Carlisle
 * Date: Feb. 23, 1994
 *
 */

#ifndef EM3D
#define EM3D

#ifndef DEPUTY
  #define COUNT(n)
  #define SAFE
  #define NTS
  #define NT
  #define BND(x,y)
#endif

extern int n_nodes; /* number of nodes (E and H) */
extern int d_nodes; /* degree of nodes */
extern int iters;   /* number of iterations */

#define ALLOC malloc
#define DIST_SPAN 6

#define assert(a) if (!a) {printf("Assertion failure\n"); exit(-1);}


typedef struct node_t {
  double value;
  struct node_t *next;
  struct node_t ** BND(__this, __auto) to_nodes; /* array of nodes pointed to */
  struct node_t ** BND(__this, __auto) from_nodes; /* array of nodes data comes from */
  double * BND(__this, __auto) coeffs; /* array of coeffs on edges */
  int from_count;
} node_t;

typedef struct graph_t {
  node_t *e_nodes;
  node_t *h_nodes;
} graph_t;

/* Perform 1 step for a nodelist */
void compute_nodes(node_t *nodelist);
#endif

