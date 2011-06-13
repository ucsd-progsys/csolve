MakeHash :: arg       (size : int(4, true))
            ret       ref(A18, 0)
            store_in  []
            store_out [A18 |-> 0: ref(A19, 0[4]), 4: int(4, true), 8: int(4, true);
                       A19 |-> ] 

main <: arg       (argc : int(4, true), argv : ref(A12, 0, {v| && [(0 < v); (BLOCK_BEGIN([v]) = v); ((v + (4 * argc)) < BLOCK_END([v]))]} ))
        ret       int(4, true)
        store    [A12 |-> 0[4]: ref(A13, 0[1], {v | && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]});
                  A13 |-> 0[1]: int(1, true)]
