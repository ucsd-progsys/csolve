malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | true}))
  ret ref(C0, 0, {v | true})
  store_in []
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
  arg       (a: ref (C0, 0, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [C0 |-> true: int (1, true, {v | true})]
  store_out []
