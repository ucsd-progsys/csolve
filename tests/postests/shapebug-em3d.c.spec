fill_table :: forall    [A9;A10]
              arg       (size : int(4, 0{1}),
                         table : ref(A9, 0[4]))
              ret       int(0, 0{1})
              store     [A9 |-> 0[4]: ref(A10, 0[4]);
                         A10 |-> 0[4]: ref(A9, 0[4])]
