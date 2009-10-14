MakeHash :: forall    [A21;A20;A19;A18]
            arg       (size : int(4, true))
            ret       ref(A18, 0)
            store_in  []
            store_out [A18 |-> 0: ref(A19, 0[4]), 4: int(4, true), 8: int(4, true);
                       A19 |-> true: ref(A20, 0);
                       A20 |-> 0: int(4, true), 4: int(4,true), 8: ref(A20, 0), 12: int(4, true)] 

main <: forall    [A13;A12]
        arg       (argc : int(4, true), argv : ref(A12, 0, {v| && [(0 < v); (BLOCK_BEGIN([v]) = v); ((v + (4 * argc)) < BLOCK_END([v]))]} ))
        ret       int(4, true)
        store_in  [A12 |-> true: ref(A13, 0[1]);
                  A13 |-> true: int(1, true)]
        store_out [A12 |-> true: ref(A13, 0[1]);
                  A13 |-> true: int(1, true)] 
