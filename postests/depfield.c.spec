test ::
  forall [A0]
  arg	 (s: ref(A0, 0))
  ret int (0, true)
  store_in [A0 |-> 0: int(4, true), 4: int(4, true)]
  store_out [A0 |-> 0: int(4, true), 4: int(4, true)]

main ::
  forall []
  arg	 ()
  ret int (0, true)
  store_in []
  store_out []
