x :: int (4, true, {v | v < 0})

loc A1 |-> 0: int(4, true),
           4: ref(A1, 0)

loc A2 |-> 0: ref(A2, 0)

ll :: ref(A1, 0)

f :: arg       (lp : ref(A1, 0))
     ret       int(0, true)
     store_in  []
     store_out []
