main ::
  forall    []
  arg       ()
  ret       int (0, true, {v | true})
  store_in  []
  store_out []

make_string ::
  forall    [A0]
  arg	    (n: int (4, true, {v | true}))
  ret	    ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> 0[1]: int (1, true, {v | true})]

new_string ::
  forall    [A0; A1]
  arg	    (n: int (4, true, {v | true}), c: int (1, true, {v | true}))
  ret	    ref (A0, 0, {v | true})
  store_in  []
  store_out [A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
             A1 |-> 0[1]: int (1, true, {v | true})]

init_string ::
  forall    [A0; A1]
  arg	    (s: ref (A0, 0, {v | true}), c: int (1, true, {v | true}))
  ret	    int (0, true, {v | true})
  store_in  [A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
             A1 |-> 0[1]: int (1, true, {v | true})]
  store_out [A0 |-> 0: int (4, true, {v | true}), 4: ref (A1, 0, {v | true});
             A1 |-> 0[1]: int (1, true, {v | true})]

new_strings ::
  forall    [A0; A1; A2]
  arg	    (n: int (4, true, {v | true}))
  ret	    ref (A1, 0, {v | true})
  store_in  []
  store_out [A1 |-> 0: ref (A1, 0, {v | true}), 4: ref (A2, 0, {v | true});
             A2 |-> 0: int (4, true, {v | true}), 4: ref (A0, 0, {v | true});
             A0 |-> 0[1]: int (1, true, {v | true})]

string_succ ::
  forall    [A0; A1; A2]
  arg	    (s: ref (A1, 4, {v | true}))
  ret	    ref (A2, 0, {v | true})
  store_in  [A1 |-> 0: ref (A1, 0, {v | true}), 4: ref (A2, 0, {v | true});
             A2 |-> 0: int (4, true, {v | true}), 4: ref (A0, 0, {v | true});
             A0 |-> 0[1]: int (1, true, {v | true})]
  store_out [A1 |-> 0: ref (A1, 0, {v | true}), 4: ref (A2, 0, {v | true});
             A2 |-> 0: int (4, true, {v | true}), 4: ref (A0, 0, {v | true});
             A0 |-> 0[1]: int (1, true, {v | true})]
