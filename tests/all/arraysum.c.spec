sum ::
  arg	    (buf: ref(A0, true, {v | true}), end: ref(A0, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  [A0 |-> true: int(4, true, {v| true})]
  store_out [A0 |-> true: int(4, true, {v| true})]

   
main ::
  arg	 ()
  ret int (4, true, {v | true})
  store_in []
  store_out []
