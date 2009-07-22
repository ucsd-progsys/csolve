alloc ::
  forall    [A0]
  arg       ()
  ret       ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> ]

access ::
  forall    [A0]
  arg       (s: ref (A0, 0, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> 0: ref (A1, 0, {v | true});
             A1 |-> 0: int (4, true, {v | true})]
  store_out [A0 |-> 0: ref (A1, 0, {v | true});
             A1 |-> 0: int (4, true, {v | true})]

main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []
