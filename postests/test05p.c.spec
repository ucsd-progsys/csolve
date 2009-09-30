make_ptr :: forall    [A6;C5]
            arg       ()
            ret       ref(C5, 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + 4]})
            store_in  []
            store_out [C5 |-> 0: ref(A6, 0);
                       A6 |-> 0: int(4, true)] 
