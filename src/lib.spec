malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | true}))
  ret ref(C0, 0, {v | true})
  store_in []
  store_out [C0 |-> ]

malloc2 ::
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
