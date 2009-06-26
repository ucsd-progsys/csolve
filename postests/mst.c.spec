mult ::
  forall    []
  arg       (p: int (4, true, {v | true}), q: int (4, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

mst_random ::
  forall    []
  arg       (seed: int (4, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

compute_dist ::
  forall    []
  arg       (i: int (4, true, {v | true}), j: int (4, true, {v | true}), numvert: int (4, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

hashfunc ::
  forall    []
  arg       (key: int (4, 0[1], {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

dealwithargs ::
  forall    []
  arg       (argc: int (4, 0[1], {v | true}), argv: ref (C0, 0[1], {v | true}))
  ret int   (4, true, {v | true})
  store_in  [C0 |-> true: ref (C1, 0[1], {v | true}); C1 |-> true: int (1, true, {v | true})]
  store_out []
