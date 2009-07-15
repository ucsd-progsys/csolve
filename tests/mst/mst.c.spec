dealwithargs ::
  forall    [A0; A1]
  arg       (argc: int (4, 0[1], {v | true}), argv: ref (A0, 0[1], {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> true: ref (A1, 0[1], {v | true}); A1 |-> true: int (1, true, {v | true})]
  store_out [A0 |-> true: ref (A1, 0[1], {v | true}); A1 |-> true: int (1, true, {v | true})]

mult ::
  forall    []
  arg       (p: int (4, true, {v | true}), q: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

mst_random ::
  forall    []
  arg       (seed: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

compute_dist ::
  forall    []
  arg       (i: int (4, true, {v | true}), j: int (4, true, {v | true}), numvert: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

hashfunc ::
  forall    []
  arg       (key: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

MakeHash ::
  forall    [A0; A1; A2]
  arg       (size: int (4, true, {v | true}))
  ret       ref (A0, 0, {v | true})
  store_in  []
  store_out [
    A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
    A1 |-> true: ref (A2, 0, {v | true});
    A2 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A2, 0, {v | true}), 12: int (4, true, {v | true})
  ]

HashLookup ::
  forall    [A0; A1; A2]
  arg       (key: int (4, true, {v | true}), hash: ref (A0, 0, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [
    A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
    A1 |-> true: ref (A2, 0, {v | true});
    A2 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A2, 0, {v | true}), 12: int (4, true, {v | true})
  ]
  store_out [
    A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
    A1 |-> true: ref (A2, 0, {v | true});
    A2 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A2, 0, {v | true}), 12: int (4, true, {v | true})
  ]

HashInsert ::
  forall    [A0; A1; A2]
  arg       (entry: int (4, true, {v | true}), key: int (4, true, {v | true}), hash: ref (A0, 0, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [
    A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
    A1 |-> true: ref (A2, 0, {v | true});
    A2 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A2, 0, {v | true}), 12: int (4, true, {v | true})
  ]
  store_out [
    A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
    A1 |-> true: ref (A2, 0, {v | true});
    A2 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A2, 0, {v | true}), 12: int (4, true, {v | true})
  ]

AddEdges ::
  forall    [A0; A1; A2; A3; A4]
  arg       (numvert: int (4, true, {v | true}), retval: ref (A3, 0, {v | true}))
  ret       int (0, true, {v | true})
  store_in  [
      A3 |-> 0: int (4, true, {v | true}), 4: ref (A4, 0, {v | true});
      A4 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A4, 0[16], {v | true}), 8[16]: ref (A2, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A2 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
      A1 |-> true: ref (A0, 0, {v | true});
      A0 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A0, 0, {v | true}), 12: int (4, true, {v | true})]
  store_out [
      A3 |-> 0: int (4, true, {v | true}), 4: ref (A4, 0, {v | true});
      A4 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A4, 0[16], {v | true}), 8[16]: ref (A2, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A2 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
      A1 |-> true: ref (A0, 0, {v | true});
      A0 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A0, 0, {v | true}), 12: int (4, true, {v | true})]

MakeGraph ::
  forall    [A0; A1; A2; A3; A4]
  arg       (numvert: int (4, true, {v | true}))
  ret       ref (A3, 0, {v | true})
  store_in  []
  store_out [
      A3 |-> 0: int (4, true, {v | true}), 4: ref (A4, 0[16], {v | true});
      A4 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A4, 0[16], {v | true}), 8[16]: ref (A2, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A2 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
      A1 |-> true: ref (A0, 0, {v | true});
      A0 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A0, 0, {v | true}), 12: int (4, true, {v | true})]

BlueRule ::
  forall    [A0; A1; A2; A3; A4]
  arg       (inserted: ref (A0, 0[16], {v | true}), vlist: ref (A0, 0[16], {v | true}))
  ret       ref (A4, 0, {v | true})
  store_in  [
      A0 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A0, 0[16], {v | true}), 8[16]: ref (A1, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A1 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true});
      A2 |-> true: ref (A3, 0, {v | true});
      A3 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A3, 0, {v | true}), 12: int (4, true, {v | true})]
  store_out [
      A0 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A0, 0[16], {v | true}), 8[16]: ref (A1, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A1 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true});
      A2 |-> true: ref (A3, 0, {v | true});
      A3 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A3, 0, {v | true}), 12: int (4, true, {v | true});
      A4 |-> 0: ref (A0, 0[16], {v | true}), 4: int (4, true, {v | true})]

ComputeMst ::
  forall    [A0; A1; A2; A3; A4]
  arg       (graph: ref (A3, 0, {v | true}), size: int (4, true, {v | true}))
  ret       int (4, true, {v | true})
  store_in  [
      A3 |-> 0: int (4, true, {v | true}), 4: ref (A4, 0[16], {v | true});
      A4 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A4, 0[16], {v | true}), 8[16]: ref (A2, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A2 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
      A1 |-> true: ref (A0, 0, {v | true});
      A0 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A0, 0, {v | true}), 12: int (4, true, {v | true})]
  store_out [
      A3 |-> 0: int (4, true, {v | true}), 4: ref (A4, 0[16], {v | true});
      A4 |-> 0[16]: int (4, true, {v | true}), 4[16]: ref (A4, 0[16], {v | true}), 8[16]: ref (A2, 0, {v | true}), 12[16]: int (4, true, {v | true});
      A2 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
      A1 |-> true: ref (A0, 0, {v | true});
      A0 |-> 0: int (4, true, {v | true}), 4: int (4, true, {v | true}), 8: ref (A0, 0, {v | true}), 12: int (4, true, {v | true})]

main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []
