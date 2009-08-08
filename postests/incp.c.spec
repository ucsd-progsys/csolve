inc ::
  forall    [C0]
  arg       (x@inc: ref (C0, 0))
  ret       int (0, true)
  store_in  [C0 |-> 0: int (4, true)]
  store_out [C0 |-> 0: int (4, true)]

main ::
  forall    []
  arg	    ()
  ret       int (0, true)
  store_in  []
  store_out []
