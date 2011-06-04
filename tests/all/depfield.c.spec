test ::
  arg	 (s: ref(A0, 0, {v | true}))
  ret int (0, true, {v | true})
  store_in [A0 |-> 0: int(4, true, {v | true}), 4: int(4, true, {v | true})]
  store_out [A0 |-> 0: int(4, true, {v | true}), 4: int(4, true, {v | true})]

main ::
  arg	 ()
  ret int (0, true, {v | true})
  store_in []
  store_out []
