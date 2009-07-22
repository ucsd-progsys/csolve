foo ::
  forall    [A0]
  arg	    (n: int(4, true))
  ret 	    ref(A0, 0) 
  store_in  []
  store_out [A0 |-> 0: int (4, true), 4: ref(A0, 0)]

bar ::
  forall []
  arg	 (n: int(4, true))
  ret int (4, true)
  store_in []
  store_out []

main ::
  forall []
  arg	 ()
  ret int (4, true)
  store_in []
  store_out []
