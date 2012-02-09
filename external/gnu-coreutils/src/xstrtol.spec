// ../lib/xstrtol.h:47: Error: No spec for extern function xstrtoumax. Autogen spec is:

xstrtoumax ::
  arg       (autoarg0 : ref(A244, 0),
             autoarg1 : ref(A245, 0),
             autoarg2 : int(4, 0{1}),
             autoarg3 : ref(A247, 0),
             autoarg4 : ref(A244, 0))
  ret       int(4, 0{1})
  store     [A244 |-> 0: int(1, 0{1});
             A245 |-> 0: ref(A246, 0);
             A246 |-> 0: int(1, 0{1});
             A247 |-> 0: int(8, 0{1})]

// ../lib/xstrtol.h:79: Error: No spec for extern function xstrtol_fatal. Autogen spec is:

xstrtol_fatal ::
  arg       (autoarg0 : int(4, 0{1}),
             autoarg1 : int(4, 0{1}),
             autoarg2 : int(1, 0{1}),
             autoarg3 : ref(A248, 0),
             autoarg4 : ref(A249, 0))
  ret       int(0, 0{1})
  store     [A248 |-> 0: ref(A249, 0),
                      4: int(4, 0{1}),
                      8: ref(A250, 0),
                      12: int(4, 0{1});
             A249 |-> 0: int(1, 0{1});
             A250 |-> 0: int(4, 0{1})]
