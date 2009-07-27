malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0, 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0 |-> ]

gen_number <:
  forall    []
  arg       (range: int (4, true, {v | 0 < v}))
  ret       int (4, true, {v | && [0 <= v; v < range]})
  store_in  []
  store_out []

// pmr: todo - fix store_in
make_table <:
  forall    [A0; A1; A2]
  arg       (size: int (4, true, {v | 0 < v}))
  ret       ref (A0, 0, {v | && [0 < v; BLOCK_END([v]) = BLOCK_BEGIN([v]) + size; v = BLOCK_BEGIN([v])]})
  store_in  [A0 |-> true: ref (A1, 0);
             A1 |-> 0: int (4, true), 4: ref (A1, 0), 8: int (4, true, {v | 0 <= v}), 12: ref (A2, 0, {v | v = 0});
             A2 |-> ]
  store_out [A0 |-> true: ref (A1, 0, {v | true});
             A1 |-> 0: int (4, true), 4: ref (A1, 0), 8: int (4, true, {v | 0 <= v}), 12: ref (A2, 0, {v | v = 0});
             A2 |-> ]

fill_table <:
  forall    [A0; A1]
  arg       (size: int (4, true, {v | 0 < v}), table: ref (A0, 0, {v | && [0 < v; BLOCK_END([v]) = BLOCK_BEGIN([v]) + size; v = BLOCK_BEGIN([v])]}))
  ret       int (0, true)
  store_in  [A0 |-> ]
  store_out [A0 |-> true: ref (A1, 0, {v | true});
             A1 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true}), 8: int (4, true, {v | true})]

fill_from_fields <:
  forall    [A0; A1; A2; A3]
  arg       (nodelist: ref (A0, 0), degree: int (4, true, {v | v = 333}))
  ret       int (0, true)
  store_in  [A0 |-> 4: ref (A0, 0, {v | true}), 12: ref (A1, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = v + 333]});
             A1 |-> true: ref (A2, 0, {v | true});
             A2 |-> 4: ref (A2, 0), 8: int (4, true), 16: ref (A3, 0);
             A3 |-> true: ref (A0, 0, {v | true})]
  store_out [A0 |-> 4: ref (A0, 0, {v | true}), 12: ref (A1, 0, {v | true});
             A1 |-> true: ref (A2, 0, {v | true});
             A2 |-> 4: ref (A2, 0, {v | true}), 8: int (4, true, {v | true}), 16: ref (A3, 0, {v | true});
             A3 |-> true: ref (A0, 0, {v | true})]

make_neighbors <:
  forall    [A0; A1; A2; A4]
  arg       (nodelist: ref (A0, 0), tablesz: int (4, true, {v | 0 < v}), table: ref (A4, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = v + tablesz]}), degree: int (4, true, {v | v = 333}))
  ret       int (0, true)
  store_in  [A0 |-> 0: int (4, true), 4: ref (A0, 0), 8: int (4, true), 12: ref (A1, 0);
             A1 |-> 0[4]: ref (A2, 0);
             A2 |-> 4: ref (A2, 0), 8: int (4, true);
             A4 |-> true: ref (A2, 0)]
  store_out [A0 |-> 0: int (4, true), 4: ref (A0, 0), 8: int (4, true), 12: ref (A1, 0);
             A1 |-> 0[4]: ref (A2, 0);
             A2 |-> 4: ref (A2, 0), 8: int (4, true);
             A4 |-> true: ref (A2, 0)]

update_from_coeffs <:
  forall    [A0; A1; A2]
  arg       (nodelist: ref (A0, 0))
  ret       int (0, true, {v | true})
  store_in  [A0 |-> 0: int (4, true), 4: ref (A0, 0, {v | true}), 8: int (4, true, {v | 0 <= v}), 16: ref (A1, 0, {v |  true}), 20: ref (A2, 0, {v | true});
             A1 |-> ;
             A2 |-> 0[4]: int (4, true, {v | true})]
  store_out []

initialize_graph ::
  forall    [A0; A1; A2; A3; A4; A5; A6; A7; A8]
  arg       ()
  ret       ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> 0: ref (A1, 0, {v | true}), 4: ref (A2, 0, {v | true});
             A1 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true}), 8: int (4, true, {v | true}), 12: ref (A4, 0, {v | true}), 16: ref (A7, 0, {v | true}), 20: ref (A8, 0, {v | true});
             A2 |-> 0: int (4, true, {v | true}), 4: ref (A2, 0, {v | true}), 8: int (4, true, {v | true}), 12: ref (A3, 0, {v | true}), 16: ref (A5, 0, {v | true}), 20: ref (A6, 0, {v | true});
             A3 |-> true: ref (A1, 0, {v | true});
             A4 |-> true: ref (A2, 0, {v | true});
             A5 |-> true: ref (A1, 0, {v | true});
             A6 |-> 0[4]: int (4, true, {v | true});
             A7 |-> true: ref (A2, 0, {v | true});
             A8 |-> 0[4]: int (4, true, {v | true})]

compute_nodes <:
  forall    [A0; A1; A2; A3; A4; A5; A6; A7; A8]
  arg       (nodelist: ref (A0, 0))
  ret       int (0, true)
  store_in  [A0 |-> 0: int (4, true), 4: ref (A0, 0), 8: int (4, true, {v | 0 <= v}), 12: ref (A1, 0), 16: ref (A2, 0), 20: ref (A4, 0);
             A1 |-> true: ref (A3, 0[4]);
             A2 |-> true: ref (A3, 0[4]);
             A3 |-> ;
             A4 |-> ]
  store_out []

main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []
