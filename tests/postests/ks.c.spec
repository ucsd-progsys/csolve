main <: forall    [A8;A7]
        arg       (argc : int(4, true),
                   argv : ref(A7, 0[4], {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = (BLOCK_BEGIN([v]) + (4 * argc))]}))
        ret       int(4, true)
        store_in  [A7 |-> 0[4]: ref(A8, 0);
                  A8 |-> 0[1]: int(1, true)]
        store_out [A7 |-> 0[4]: ref(A8, 0);
                  A8 |-> 0[1]: int(1, true)] 
