main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []

adpcm_coder ::
  forall    [A0; A1; A2]
  arg       (nsample: int (4, true, {v | true}), indata: ref (A0, 0, {v | true}), outdata: ref (A1, 0, {v | true}), state: ref (A2, 0, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> true: int (2, true, {v | true}); A1 |-> true: int (1, true, {v | true}); A2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
  store_out [A0 |-> true: int (2, true, {v | true}); A1 |-> true: int (1, true, {v | true}); A2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]

adpcm_decoder ::
  forall    [A0; A1; A2]
  arg       (nsample: int (4, true, {v | true}), indata: ref (A1, 0, {v | true}), outdata: ref (A0, 0, {v | true}), state: ref (A2, 0, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> true: int (2, true, {v | true}); A1 |-> true: int (1, true, {v | true}); A2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
  store_out [A0 |-> true: int (2, true, {v | true}); A1 |-> true: int (1, true, {v | true}); A2 |-> 0: int (2, true, {v | true}), 2: int (1, true, {v | true})]
