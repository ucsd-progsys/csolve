// ../lib/xalloc.h:73: Error: No spec for extern function xstrdup. Autogen spec is: TODO strengthen with invariants about length

xstrdup ::
  arg       (str : ref(A0, 0))
  ret       ref(A1, 0)
  store_in  [A0 |-> 0: int(1, 0{1})]
  store_out [A0 |-> 0: int(1, 0{1});
             A1 |-> 0: int(1, 0{1})]

// ../lib/xalloc.h:70: Error: No spec for extern function x2realloc. Autogen spec is:

x2realloc ::
  arg       (p : ref(A234, 0),
             pn : ref(A235, 0))
  ret       ref(A234, 0)
  store     [A234 |-> 0: int(0, 0{1});
             A235 |-> 0: int(4, 0{1})]

// ../lib/xalloc.h:121: Error: No spec for extern function xnmalloc. Autogen spec is:

xnmalloc ::
  arg       (n : int(4, 0{1}),
             s : int(4, 0{1}))
  ret       ref(A236, 0)
  store_in  []
  store_out [A236 |-> 0: int(0, 0{1})]

// ../lib/xalloc.h:125: Error: No spec for extern function x2nrealloc. Autogen spec is:

x2nrealloc ::
  arg       (p : ref(A237, 0),
             pn : ref(A238, 0),
             s : int(4, 0{1}))
  ret       ref(A237, 0)
  store     [A237 |-> 0: int(0, 0{1});
             A238 |-> 0: int(4, 0{1})]


