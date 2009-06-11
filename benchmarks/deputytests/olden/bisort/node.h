/* For copyright information, see olden_v1.0/COPYRIGHT */

/* =============== NODE STRUCTURE =================== */

struct node { 
  int value;
  struct node *left;
  struct node *right;
  };

typedef struct node HANDLE;

#ifdef PLAIN
#include <time.h>
#define local
#define mymalloc malloc
#define CMMD_node_timer_clear(x)  (void)0
#define TIMESTART(clk) {clk=(double)clock();}
#define TIMESTOP(clk) {clk=1000000.0 * ((double)clock()-(clk))/CLOCKS_PER_SEC;}
extern double wallclock;
#define timer_start(x) TIMESTART(wallclock)
#define timer_stop(x) TIMESTOP(wallclock)
#define timer_elapsed(x) (wallclock / 1000.0)
#define chatting printf
#define NOTEST() (void)0
#define RETEST() (void)0

extern int __NumNodes, __NDim;
#endif

#define NIL ((HANDLE *) 0)
#ifdef FUTURES
#define makenode(procid) ALLOC(procid,sizeof(struct node))
#else
#define makenode(procid) mymalloc(sizeof(struct node))
#endif

#include "../../nodeputy.h"
#include <stdio.h>

//stdlib.h defines a random(), but bisort defines a different one.
//Turn off libc's version
#define random stdlib_random
#include <stdlib.h>
#undef random
