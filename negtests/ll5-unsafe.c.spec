foo ::
  forall    [A0]
  arg       (n: int (4, true, {v | true}))
  ret       ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> 0: int (4, true, {v | true}), 4: ref (A0, 0, {v | true})]

bar ::
  forall    []
  arg       (n: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []
