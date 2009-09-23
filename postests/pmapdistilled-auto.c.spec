main :: forall    []
        arg       ()
        ret       int(0, true)
        store_in  []
        store_out [] 

page_map :: forall    [A23;A24;A25]
            arg       (srcenv : ref(A23, 0), srcvp : int(4, 0[1]), dstenv : ref(A23, 0), dstvp : int(4, 0[1]), envs : ref(A24, 0), pages : ref(A25, 0[4]), page_protected : ref(A25, 0[4]))
            ret       int(4, true)
            store_in  [A25 |-> 0[4]: int(4, true);
                      A24 |-> 0: ref(A23, 0);
                      A23 |-> 0: int(4, true), 4: ref(A23, 0), 8: ref(A23, 0), 12[4]: int(4, true)]
            store_out [A25 |-> 0[4]: int(4, true);
                      A24 |-> 0: ref(A23, 0);
                      A23 |-> 0: int(4, true), 4: ref(A23, 0), 8: ref(A23, 0), 12[4]: int(4, true)] 

page_unmap :: forall    [A20;A21;A22]
              arg       (env : ref(A20, 0), vp : int(4, 0[1]), envs : ref(A21, 0), pages : ref(A22, 0[4]), page_protected : ref(A22, 0[4]))
              ret       int(0, true)
              store_in  [A22 |-> 0[4]: int(4, true);
                        A21 |-> 0: ref(A20, 0);
                        A20 |-> 0: int(4, true), 4: ref(A20, 0), 8: ref(A20, 0), 12[4]: int(4, true)]
              store_out [A22 |-> 0[4]: int(4, true);
                        A21 |-> 0: ref(A20, 0);
                        A20 |-> 0: int(4, true), 4: ref(A20, 0), 8: ref(A20, 0), 12[4]: int(4, true)] 

page_alloc :: forall    [A17;A18;A19]
              arg       (env : ref(A17, 0), vp : int(4, true), envs : ref(A18, 0), pages : ref(A19, 0[4]), page_protected : ref(A19, 0[4]))
              ret       int(4, true)
              store_in  [A19 |-> 0[4]: int(4, true);
                        A18 |-> 0: ref(A17, 0);
                        A17 |-> 0: int(4, true), 4: ref(A17, 0), 8: ref(A17, 0), 12[4]: int(4, true)]
              store_out [A19 |-> 0[4]: int(4, true);
                        A18 |-> 0: ref(A17, 0);
                        A17 |-> 0: int(4, true), 4: ref(A17, 0), 8: ref(A17, 0), 12[4]: int(4, true)] 

env_free :: forall    [A14;A15;A16]
            arg       (env : ref(A14, 0), envs : ref(A15, 0), pages : ref(A16, 0[4]), page_protected : ref(A16, 0[4]))
            ret       int(0, true)
            store_in  [A16 |-> 0[4]: int(4, true);
                      A15 |-> 0: ref(A14, 0);
                      A14 |-> 0: int(4, true), 4: ref(A14, 0), 8: ref(A14, 0), 12[4]: int(4, true)]
            store_out [A16 |-> 0[4]: int(4, true);
                      A15 |-> 0: ref(A14, 0);
                      A14 |-> 0: int(4, true), 4: ref(A14, 0), 8: ref(A14, 0), 12[4]: int(4, true)] 

env_alloc :: forall    [A11;A10;A12;A13]
             arg       (envs : ref(A10, 0), pages : ref(A12, 0[4]), page_protected : ref(A12, 0[4]))
             ret       ref(A13, 0)
             store_in  [A12 |-> 0[4]: int(4, true);
                       A10 |-> 0: ref(A11, 0);
                       A11 |-> 0: int(4, true), 4: ref(A11, 0), 8: ref(A11, 0), 12[4]: int(4, true)]
             store_out [A13 |-> 0: int(4, true), 4: ref(A13, 0), 8: ref(A13, 0), 12[4]: int(4, true);
                       A12 |-> 0[4]: int(4, true);
                       A10 |-> 0: ref(A11, 0);
                       A11 |-> 0: int(4, true), 4: ref(A11, 0), 8: ref(A11, 0), 12[4]: int(4, true)] 

mem_check :: forall    [A8;A7;A9]
             arg       (envs : ref(A7, 0), pages : ref(A9, 0[4]), page_protected : ref(A9, 0[4]))
             ret       int(0, true)
             store_in  [A9 |-> 0[4]: int(4, true);
                       A7 |-> 0: ref(A8, 0);
                       A8 |-> 0: int(4, true), 4: ref(A8, 0), 8: ref(A8, 0), 12[4]: int(4, true)]
             store_out [A9 |-> 0[4]: int(4, true);
                       A7 |-> 0: ref(A8, 0);
                       A8 |-> 0: int(4, true), 4: ref(A8, 0), 8: ref(A8, 0), 12[4]: int(4, true)] 

env_check :: forall    [A4;A5;A6]
             arg       (env : ref(A4, 0), envs : ref(A5, 0), pages : ref(A6, 0[4]), page_protected : ref(A6, 0[4]))
             ret       int(0, true)
             store_in  [A6 |-> 0[4]: int(4, true);
                       A5 |-> 0: ref(A4, 0);
                       A4 |-> 0: int(4, true), 4: ref(A4, 0), 8: ref(A4, 0), 12[4]: int(4, true)]
             store_out [A6 |-> 0[4]: int(4, true);
                       A5 |-> 0: ref(A4, 0);
                       A4 |-> 0: int(4, true), 4: ref(A4, 0), 8: ref(A4, 0), 12[4]: int(4, true)] 

page_decref :: forall    [A3]
               arg       (ppno : int(4, true), pages : ref(A3, 0[4]), page_protected : ref(A3, 0[4]))
               ret       int(0, true)
               store_in  [A3 |-> 0[4]: int(4, true)]
               store_out [A3 |-> 0[4]: int(4, true)] 

is_page_protected :: forall    [A2]
                     arg       (ppno : int(4, true), pages : ref(A2, 0[4]), page_protected : ref(A2, 0[4]))
                     ret       int(4, true)
                     store_in  [A2 |-> 0[4]: int(4, true)]
                     store_out [A2 |-> 0[4]: int(4, true)] 

page_free :: forall    [A1]
             arg       (ppno : int(4, true), pages : ref(A1, 0[4]), page_protected : ref(A1, 0[4]))
             ret       int(4, true)
             store_in  [A1 |-> 0[4]: int(4, true)]
             store_out [A1 |-> 0[4]: int(4, true)] 

page_getfree :: forall    [A0]
                arg       (pages : ref(A0, 0[4]))
                ret       int(4, true)
                store_in  [A0 |-> 0[4]: int(4, true)]
                store_out [A0 |-> 0[4]: int(4, true)] 

