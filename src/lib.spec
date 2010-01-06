malloc ::
  forall [C0]
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0, 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0 |-> ]

free ::
  forall [A0]
  arg (ptr: ref (A0, true, {v | 0 < v}))
  ret int(0, true, {v | true})
  store_in [A0 |-> ]
  store_out [A0 |-> ]

atbegin::
  forall    [A0]
  arg       (x: ref (A0, true, {v| && [(BLOCK_BEGIN([v]) = v)]}))
  ret int   (4, true, {v | true})
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

validp8 ::
  forall    [A0]
  arg       (x: ref (A0, true, {v| && [(0 < v); ((BLOCK_BEGIN([v]) + 8) = BLOCK_END([v]))]}))
  ret int   (4, true, {v | true})
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

getc :: forall    [A11]
        arg       (x0 : ref(A11, 0))
        ret       int(4, 0{1}, {v | &&[0 <= v; v < 256]})
        store_in  [A11 |-> 0: int(4, 0{1})]
        store_out [A11 |-> 0: int(4, 0{1})]

validptr ::
  forall    [A0]
  arg       (x: ref (A0, true, {v| && [(0 < v); (BLOCK_BEGIN([v]) <= v) ; (v < BLOCK_END([v]))]}))
  ret int   (0, true)
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

nondetpos ::
  forall    []
  arg       ()
  ret int   (4, 0[1], {v | v > 0})
  store_in  []
  store_out []

nondetnn ::
  forall    []
  arg       ()
  ret int   (4, 0[1], {v | v >= 0})
  store_in  []
  store_out []

nondet ::
  forall    []
  arg       ()
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

dummyassert ::
  forall    []
  arg 	    (b: int(4, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []


assert ::
  forall    []
  arg 	    (b: int(4, true, {v | v != 0}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

csolve_exit ::
  forall    []
  arg       (status: int (4, true, {v | true}))
  ret int   (4, true, {v | 0=1})
  store_in  []
  store_out []

exit ::
  forall    []
  arg       (status: int (4, true, {v | true}))
  ret int   (0, true, {v | 0=1})
  store_in  []
  store_out []

atoi ::
  forall    [A0]
  arg       (a: ref (A0, 0[1], {v | true}))
  ret       int (4, true, {v | true})
  store_in  [A0 |-> 0[1]: int (1, true, {v | true})]
  store_out [A0 |-> 0[1]: int (1, true, {v | true})]

random ::
  forall    []
  arg       ()
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

pow ::
  forall    []
  arg       (base: int (8, true), exp: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

clock ::
  forall    []
  arg       ()
  ret       int (4, true)
  store_in  []
  store_out []

sqrt ::
  forall    []
  arg       (n: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

fabs ::
  forall    []
  arg       (n: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

bor ::
  forall    []
  arg       (a: int (4, true, {v | v >= 0}), b: int (4, true, {v | v >= 0}))
  ret       int (4, true, {v | && [a <= v; b <= v; v <= a + b]})
  store_in  []
  store_out []

band ::
  forall    []
  arg       (a: int (4, true), b: int (4, true, {v | v >= 0}))
  ret       int (4, true, {v | && [v <= a; v <= b; 0 <= v]})
  store_in  []
  store_out []

assume ::
  forall    []
  arg 	    (b: int(4, true))
  ret int   (4, true, {v | b = 1})
  store_in  []
  store_out []

read ::
  forall    [A1]
  arg       (fd: int (4, true), dst: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

write ::
  forall    [A1]
  arg       (fd: int (4, true), src: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

perror ::
  forall    [A1]
  arg       (err: ref (A1, 0[1]))
  ret       int (0, true)
  store_in  [A1 |-> 0[1]: int (1, true)]
  store_out [A1 |-> 0[1]: int (1, true)]

strlen ::
  forall    [A1]
  arg       (str: ref (A1, 0[1]))
  ret       int (4, 0[1])
  store     [A1 |-> 0[1]: int (1, true)]
