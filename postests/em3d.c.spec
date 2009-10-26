// I think we need this because em3d mallocs with size 0 sometimes
malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0, 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0 |-> ]

// This seems to have been important at some time
// make_table <:
//   forall    [A0; A1; A2; A3]
//   arg       (size: int (4, true, {v | 0 < v}))
//   ret       ref (A0, 0, {v | && [0 < v; BLOCK_END([v]) = BLOCK_BEGIN([v]) + size * 4; v = BLOCK_BEGIN([v])]})
//   store     [A0 |-> true: ref (A1, 0);
//              A1 |-> 0[24]: int (4, true), 4[24]: ref (A1, 0), 8[24]: int (4, true, {v | 0 <= v}), 12[24]: ref (A2, 0), 16[24]: ref (A2, 0), 20[24]: ref (A3, 0);
//              A2 |-> true: ref (A1, 0[24]);
//              A3 |-> true: int (4, true)]
