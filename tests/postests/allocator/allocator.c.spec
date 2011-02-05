init :: forall    [A8]
              arg       (size : int (4, true), m : ref(A8, 8[1]))
              ret       int(0, 0)
              store     [A8 |-> 0: int(4, 0{1}),
                                4: ref(A8, 0),
                                8[1]: int(1, 0{1})]

new_region :: forall    [A8]
              arg       (size : int (4, true))
              ret       ref(A8, 8)
              store     [A8 |-> 0: int(4, 0{1}),
                                4: ref(A8, 0),
                                8[1]: int(1, 0{1})]

pool_alloc :: forall    [A10;A9]
                 arg       (p : ref(A9, 0))
                 ret       ref(A10, 8)
                 store     [A9 |-> 0: int(4, 0{1}),
                                   4: ref(A10, 0),
                                   8: ref(A9, 0);
                            A10 |-> 4: ref(A10, 0),
                                    0: int(4, 0{1}),
                                    8[1]: int(1, 0{1})]

alloc :: forall    [A10;A9]
         arg       (freelist : ref(A9, 0),
                    size     : int(4, true))
         ret       ref(A10, 8)
         store     [A9 |-> 0: int(4, 0{1}),
                           4: ref(A10, 0),
                           8: ref(A9, 0);
                    A10 |-> 4: ref(A10, 0),
                            0: int(4, 0{1}),
                            8[1]: int(1, 0{1})]

dealloc :: forall    [A10;A9]
           arg       (freelist : ref(A9, 0),
                      mem      : ref(A10, 8))
           ret       int(0, 0)
           store     [A9 |-> 0: int(4, 0{1}),
                             4: ref(A10, 0),
                             8: ref(A9, 0);
                      A10 |-> 4: ref(A10, 0),
                              0: int(4, 0{1}),
                              8[1]: int(1, 0{1})]

check_invariants :: forall    [A10;A9]
                 arg       (fl : ref(A9, 0))
                 ret       int(0, true)
                 store     [A9 |-> 0: int(4, 0{1}),
                                   4: ref(A10, 0),
                                   8: ref(A9, 0);
                            A10 |-> 4: ref(A10, 0),
                                    0: int(4, 0{1}),
                                    8[1]: int(1, 0{1})]
