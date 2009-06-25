adpcm_coder ::
  forall []
  arg (indata: ref (C0, 0, {v | true}), outdata: ref (C1, 0, {v | true}), nsample: int (4, true, {v | true}), state: ref (C2, 0, {v | true}))
  ret int (0, true, {v | true})
  store_in [C0 |-> 0[2]: int (2, true, {v | true}); C1 |-> 0[1]: int (1, true, {v | true}); C2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
  store_out [C0 |-> 0[2]: int (2, true, {v | true}); C1 |-> 0[1]: int (1, true, {v | true}); C2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]

adpcm_decoder ::
  forall []
  arg (indata: ref (C0, 0, {v | true}), outdata: ref (C1, 0, {v | true}), nsample: int (4, true, {v | true}), state: ref (C2, 0, {v | true}))
  ret int (0, true, {v | true})
  store_in [C0 |-> 0[1]: int (1, true, {v | true}); C1 |-> 0[2]: int (2, true, {v | true}); C2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
  store_out [C0 |-> 0[1]: int (1, true, {v | true}); C1 |-> 0[2]: int (2, true, {v | true}); C2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
