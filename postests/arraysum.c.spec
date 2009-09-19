sum ::
  forall    [A0]
  arg	    (buf: ref(A0, 0[4]), end: ref(A0, 0[4]))
  ret int   (4, true)
  store_in  [A0 |-> 0[4]: int(4, true)]
  store_out [A0 |-> 0[4]: int(4, true)]

   
main ::
  forall []
  arg	 ()
  ret int (4, true)
  store_in []
  store_out []
