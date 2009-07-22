main ::
  forall    []
  arg       ()
  ret       int (0, true)
  store_in  []
  store_out []

make_string ::
  forall    [A0]
  arg	    (n: int (4, true))
  ret	    ref (A0, 0)
  store_in  []
  store_out [A0 |-> 0[1]: int (1, true)]

new_string ::
  forall    [A0; A1]
  arg	    (n: int (4, true), c: int (1, true))
  ret	    ref (A0, 0)
  store_in  []
  store_out [A0 |-> 0: int (4, true), 4: ref (A1, 0);
             A1 |-> 0[1]: int (1, true)]

init_string ::
  forall    [A0; A1]
  arg	    (s: ref (A0, 0), c: int (1, true))
  ret	    int (0, true)
  store_in  [A0 |-> 0: int (4, true), 4: ref (A1, 0);
             A1 |-> 0[1]: int (1, true)]
  store_out [A0 |-> 0: int (4, true), 4: ref (A1, 0);
             A1 |-> 0[1]: int (1, true)]

new_strings ::
  forall    [A0; A1; A2]
  arg	    (n: int (4, true))
  ret	    ref (A1, 0)
  store_in  []
  store_out [A1 |-> 0: ref (A1, 0), 4: ref (A2, 0);
             A2 |-> 0: int (4, true), 4: ref (A0, 0);
             A0 |-> 0[1]: int (1, true)]

string_succ ::
  forall    [A0; A1; A2]
  arg	    (s: ref (A1, 4))
  ret	    ref (A2, 0)
  store_in  [A1 |-> 0: ref (A1, 0), 4: ref (A2, 0);
             A2 |-> 0: int (4, true), 4: ref (A0, 0);
             A0 |-> 0[1]: int (1, true)]
  store_out [A1 |-> 0: ref (A1, 0), 4: ref (A2, 0);
             A2 |-> 0: int (4, true), 4: ref (A0, 0);
             A0 |-> 0[1]: int (1, true)]
