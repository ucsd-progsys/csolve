AddEdges ::
  forall    [A0]
  arg       (retval: ref (A0, 0, {v | true}), numvert: int (4, true, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> 0: int (4, 0[1], {v | true}), 4[4]: ref (A1, 0, {v | true}); A1 |-> true: ref (A1, true, {v | true})]
  store_out [A0 |-> 0: int (4, 0[1], {v | true}), 4[4]: ref (A1, 0, {v | true}); A1 |-> true: ref (A1, true, {v | true})]

MakeGraph ::
  forall    [A0; A1]
  arg       (numvert: int (4, 0[1], {v | true}))
  ret       ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> 0: int (4, 0[1], {v | true}), 4[4]: ref (A1, 0, {v | true}); A1 |-> true: ref (A1, true, {v | true})]

BlueRule ::
  forall    [A0]
  arg       (inserted: ref (A0, true, {v | true}), vlist: ref (A0, true, {v | true}))
  ret       ref (A0, true, {v | true})
  store_in  [A0 |-> true: ref (A0, true, {v | true})]
  store_out [A0 |-> true: ref (A0, true, {v | true})]

ComputeMst ::
  forall    [A0]
  arg       (graph: ref (A0, 0, {v | true}), numvert: int (4, 0[1], {v | true}))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> 0: int (4, 0[1], {v | true}), 4[4]: ref (A1, true, {v | true}); A1 |-> true: ref (A1, true, {v | true})]
  store_out [A0 |-> 0: int (4, 0[1], {v | true}), 4[4]: ref (A1, true, {v | true}); A1 |-> true: ref (A1, true, {v | true})]

dealwithargs ::
  forall    []
  arg       (argc: int (4, 0[1], {v | true}), argv: ref (A0, 0[1], {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> true: ref (A1, 0[1], {v | true}); A1 |-> true: int (1, true, {v | true})]
  store_out [A0 |-> true: ref (A1, 0[1], {v | true}); A1 |-> true: int (1, true, {v | true})]
