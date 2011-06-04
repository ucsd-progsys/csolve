call_it <: forall []
           arg    (f: ref(A1, 0, {v | && [v > 0; BLOCK_BEGIN([v]) = v; v < BLOCK_END([v])]}))
           ret    int(0, 0{1})
           store  [A1 |-> forall []
                          arg    ()
                          ret    int(4, 0[1], {v | v > 0})
                          store  []]