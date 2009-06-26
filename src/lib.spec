malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | 0 < v}))
  ret ref(C0, 0, {v | && [BBEG([v]) = v; BEND([v]) = v + sz]})
  store_in []
  store_out [C0 |-> ]

validptr ::
  forall    [C0]
  arg       (x: ref (C0, true, {v| && [(BBEG([v]) <= v); (v < BEND([v]))]}))
  ret int   (4, true, {v | true})
  store_in  [C0 |-> ]
  store_out [C0 |-> ]

nondet ::
  forall    []
  arg       ()
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

assert ::
  forall    []
  arg 	    (b: int(4, true, {v | v != 0}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

exit ::
  forall    []
  arg       (status: int (4, true, {v | true}))
  ret int   (0, true, {v | true})
  store_in  []
  store_out []

atoi ::
  forall    [C0]
  arg       (a: ref (C0, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [C0 |-> true: int (1, true, {v | true})]
  store_out []
