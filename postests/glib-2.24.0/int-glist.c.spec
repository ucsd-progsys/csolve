assert_sorted ::
  forall [A0]
  arg    (hd: ref (A0, 0))
  ret    int (0, true)
  store  [A0 |-> 0: final int (4, true),
                 4: ref (A0, 0, {v | (v != 0) -> && [v > 0;
                                                     v = BLOCK_BEGIN([v]);
                                                     BLOCK_END([v]) = (BLOCK_BEGIN([v]) + 12);
                                                     A0#Ix#0 <= (DEREF([v]) : int)]}),
                 8: ref (A0, 0, {v | (v != 0) -> && [v > 0;
                                                     v = BLOCK_BEGIN([v]);
                                                     BLOCK_END([v]) = (BLOCK_BEGIN([v]) + 12);
                                                     A0#Ix#0 >= (DEREF([v]) : int)]})]