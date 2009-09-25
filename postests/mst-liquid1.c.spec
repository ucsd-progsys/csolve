dealwithargs :: forall    [A43;A42]
                arg       (argc : int(4, true), argv : ref(A42, 0[4]))
                ret       int(4, true)
                store_in  [A42 |-> true: ref(A43, 0[1]);
                          A43 |-> true: int(1, true)]
                store_out [A42 |-> true: ref(A43, 0[1]);
                          A43 |-> true: int(1, true)] 

HashDelete :: forall    [A41;A40;A39;A38]
              arg       (key : int(4, true), hash : ref(A38, 0))
              ret       int(0, true)
              store_in  [A38 |-> 0: ref(A39, 0[4]), 4: int(4, true), 8: int(4, true);
                        A39 |-> true: ref(A40, 0);
                        A40 |-> 0: int(4, true), 4: ref(A41, 0), 8: ref(A40, 0), 12: int(4, true);
                        A41 |-> 0: int(0, true)]
              store_out [A38 |-> 0: ref(A39, 0[4]), 4: int(4, true), 8: int(4, true);
                        A39 |-> true: ref(A40, 0);
                        A40 |-> 0: int(4, true), 4: ref(A41, 0), 8: ref(A40, 0), 12: int(4, true);
                        A41 |-> 0: int(0, true)] 

HashInsert :: forall    [A34;A37;A36;A35]
              arg       (entry : ref(A34, 0), key : int(4, true), hash : ref(A35, 0))
              ret       int(0, true)
              store_in  [A35 |-> 0: ref(A36, 0[4]), 4: int(4, true), 8: int(4, true);
                        A36 |-> true: ref(A37, 0);
                        A37 |-> 0: int(4, true), 4: ref(A34, 0), 8: ref(A37, 0), 12: int(4, true);
                        A34 |-> 0: int(0, true)]
              store_out [A35 |-> 0: ref(A36, 0[4]), 4: int(4, true), 8: int(4, true);
                        A36 |-> true: ref(A37, 0);
                        A37 |-> 0: int(4, true), 4: ref(A34, 0), 8: ref(A37, 0), 12: int(4, true);
                        A34 |-> 0: int(0, true)] 

HashLookup :: forall    [A33;A32;A31;A30]
              arg       (key : int(4, true), hash : ref(A30, 0))
              ret       ref(A33, 0)
              store_in  [A30 |-> 0: ref(A31, 0[4]), 4: int(4, true), 8: int(4, true);
                        A31 |-> true: ref(A32, 0);
                        A32 |-> 0: int(4, true), 4: ref(A33, 0), 8: ref(A32, 0), 12: int(4, true);
                        A33 |-> 0: int(0, true)]
              store_out [A30 |-> 0: ref(A31, 0[4]), 4: int(4, true), 8: int(4, true);
                        A31 |-> true: ref(A32, 0);
                        A32 |-> 0: int(4, true), 4: ref(A33, 0), 8: ref(A32, 0), 12: int(4, true);
                        A33 |-> 0: int(0, true)] 

MakeHash :: forall    [A29;A28;A27;A26]
            arg       (size : int(4, true))
            ret       ref(A26, 0)
            store_in  []
            store_out [A26 |-> 0: ref(A27, 0[4]), 4: int(4, true), 8: int(4, true);
                      A27 |-> true: ref(A28, 0);
                      A28 |-> 0: int(4, true), 4: ref(A29, 0), 8: ref(A28, 0), 12: int(4, true);
                      A29 |-> 0: int(0, true)] 

MakeGraph :: forall    [A25;A24;A23;A22;A21;A20]
             arg       (numvert : int(4, true))
             ret       ref(A20, 0)
             store_in  []
             store_out [A20 |-> 0: ref(A21, 0);
                       A21 |-> 0: int(4, true), 4: ref(A21, 0), 8: ref(A22, 0), 12: int(4, true);
                       A22 |-> 0: ref(A23, 0[4]), 4: int(4, true), 8: int(4, true);
                       A23 |-> true: ref(A24, 0);
                       A24 |-> 0: int(4, true), 4: ref(A25, 0), 8: ref(A24, 0), 12: int(4, true);
                       A25 |-> 0: int(0, true)] 

AddEdges :: forall    [A19;A18;A17;A16;A15;A14]
            arg       (retval : ref(A14, 0), numvert : int(4, true))
            ret       int(0, true)
            store_in  [A14 |-> 0: ref(A15, 0);
                      A15 |-> 0: int(4, true), 4: ref(A15, 0), 8: ref(A16, 0), 12: int(4, true);
                      A16 |-> 0: ref(A17, 0[4]), 4: int(4, true), 8: int(4, true);
                      A17 |-> true: ref(A18, 0);
                      A18 |-> 0: int(4, true), 4: ref(A19, 0), 8: ref(A18, 0), 12: int(4, true);
                      A19 |-> 0: int(0, true)]
            store_out [A14 |-> 0: ref(A15, 0);
                      A15 |-> 0: int(4, true), 4: ref(A15, 0), 8: ref(A16, 0), 12: int(4, true);
                      A16 |-> 0: ref(A17, 0[4]), 4: int(4, true), 8: int(4, true);
                      A17 |-> true: ref(A18, 0);
                      A18 |-> 0: int(4, true), 4: ref(A19, 0), 8: ref(A18, 0), 12: int(4, true);
                      A19 |-> 0: int(0, true)] 

hashfunc :: forall    []
            arg       (HashRange : int(4, true), key : int(4, true))
            ret       int(4, true)
            store_in  []
            store_out [] 

compute_dist :: forall    []
                arg       (i : int(4, true), j : int(4, true), numvert : int(4, true))
                ret       int(4, true)
                store_in  []
                store_out [] 

mst_random :: forall    []
              arg       (seed : int(4, true))
              ret       int(4, true)
              store_in  []
              store_out [] 

mult :: forall    []
        arg       (p : int(4, true), q : int(4, true))
        ret       int(4, true)
        store_in  []
        store_out [] 

main :: forall    [A13;A12]
        arg       (argc : int(4, true), argv : ref(A12, 0[4]))
        ret       int(4, true)
        store_in  [A12 |-> true: ref(A13, 0);
                  A13 |-> true: int(1, true)]
        store_out [A12 |-> true: ref(A13, 0);
                  A13 |-> true: int(1, true)] 

ComputeMst :: forall    [A11;A10;A9;A8;A7;A6]
              arg       (graph : ref(A6, 0), numvert : int(4, true))
              ret       int(4, true)
              store_in  [A6 |-> 0: ref(A7, 0);
                        A7 |-> 0: int(4, true), 4: ref(A7, 0), 8: ref(A8, 0), 12: int(4, true);
                        A8 |-> 0: ref(A9, 0[4]), 4: int(4, true), 8: int(4, true);
                        A9 |-> true: ref(A10, 0);
                        A10 |-> 0: int(4, true), 4: ref(A11, 0), 8: ref(A10, 0), 12: int(4, true);
                        A11 |-> 0: int(0, true)]
              store_out [A6 |-> 0: ref(A7, 0);
                        A7 |-> 0: int(4, true), 4: ref(A7, 0), 8: ref(A8, 0), 12: int(4, true);
                        A8 |-> 0: ref(A9, 0[4]), 4: int(4, true), 8: int(4, true);
                        A9 |-> true: ref(A10, 0);
                        A10 |-> 0: int(4, true), 4: ref(A11, 0), 8: ref(A10, 0), 12: int(4, true);
                        A11 |-> 0: int(0, true)] 

BlueRule :: forall    [A4;A3;A2;A1;A0;A5]
            arg       (inserted : ref(A0, 0), vlist : ref(A0, 0))
            ret       ref(A5, 0)
            store_in  [A0 |-> 0: int(4, true), 4: ref(A0, 0), 8: ref(A1, 0), 12: int(4, true);
                      A1 |-> 0: ref(A2, 0[4]), 4: int(4, true), 8: int(4, true);
                      A2 |-> true: ref(A3, 0);
                      A3 |-> 0: int(4, true), 4: ref(A4, 0), 8: ref(A3, 0), 12: int(4, true);
                      A4 |-> 0: int(0, true)]
            store_out [A5 |-> 0: ref(A0, 0), 4: int(4, true);
                      A0 |-> 0: int(4, true), 4: ref(A0, 0), 8: ref(A1, 0), 12: int(4, true);
                      A1 |-> 0: ref(A2, 0[4]), 4: int(4, true), 8: int(4, true);
                      A2 |-> true: ref(A3, 0);
                      A3 |-> 0: int(4, true), 4: ref(A4, 0), 8: ref(A3, 0), 12: int(4, true);
                      A4 |-> 0: int(0, true)] 

