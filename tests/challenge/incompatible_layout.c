// Taken from coreutils/src/tsort.c

#include <stdlib.h>
#include <csolve.h>

struct successor {
  struct successor *next;
  struct item *suc;
};

struct item {
  struct successor *top;
  const char * ARRAY str;
};

static void
detect_loop (struct item *k,
             struct item **head,
             struct item **zeros)
{
    struct successor **p = &k->top;
    struct successor *dp = *p;    // RJ: deref in while (*p)
                                  // not getting 3-addressed by CIL

    while (dp) {                  // RJ: deref 3-addr issue
        p = &(*p)->next;
        dp = *p;                  // RJ: deref 3-addr issue
    }
    // Can be fixed by
    // 1. Peeling off the first iteration
    // 2. Making variables p2, dp2 to hold values of p, dp after the first
    //    iteration, preventing the pointers in the first and later iterations
    //    from being unified
}

void main () {
    detect_loop (NULL, NULL, NULL);
}
