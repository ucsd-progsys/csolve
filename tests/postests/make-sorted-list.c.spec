copy :: arg    (hd : ref(A5, 0))
        ret    ref(A6, 0)
        store  [A5 |-> 0: int(4, 0{1}),
                          4: ref(A5, 0);
                A6 |-> 0: int(4, 0{1}),
                       4: ref(A6, 0)]
