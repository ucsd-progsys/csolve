main <: arg       (argc : int(4, true), argv : ref(A12, 0, {v| && [(0 < v); (BLOCK_BEGIN([v]) = v); ((v + (4 * argc)) < BLOCK_END([v]))]} ))
        ret       int(4, true)
        store_in  [A12 |-> true: ref(A13, 0[1]);
                  A13 |-> true: int(1, true)]
        store_out [A12 |-> true: ref(A13, 0[1]);
                  A13 |-> true: int(1, true)] 
    
