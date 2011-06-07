malloc ::
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0[A0], 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0[A0] |-> ]

free ::
  arg (p: ref (A0, true, {v | 0 < v}))
  ret int(0, true, {v | true})
  store_in [A0 |-> ]
  store_out [A0 |-> ]

atbegin::
  arg       (x: ref (A0, true, {v | && [(BLOCK_BEGIN([v]) = v)]}))
  ret int   (4, true, {v | true})
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

validp8 ::
  arg       (x: ref (A0, true, {v | && [(0 < v); ((BLOCK_BEGIN([v]) + 8) = BLOCK_END([v]))]}))
  ret int   (4, true, {v | true})
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

getc :: arg       (x0 : ref(A11, 0))
        ret       int(4, 0{1}, {v | &&[(0-1) <= v; v < 256]})
        store_in  [A11 |-> ]
        store_out [A11 |-> ]

_IO_getc ::
        arg       (x0 : ref(A22, 0))
        ret       int(4, 0{1}, {v | &&[(0-1) <= v; v < 256]})
        store_in  [A22 |-> ]
        store_out [A22 |-> ]

validptr ::
  arg       (x: ref (A0, true, {v | && [(0 < v); (BLOCK_BEGIN([v]) <= v) ; (v < BLOCK_END([v]))]}))
  ret int   (0, true)
  store_in  [A0 |-> ]
  store_out [A0 |-> ]

nondetpos ::
  arg       ()
  ret int   (4, 1[1], {v | v > 0})
  store_in  []
  store_out []

nondetnn ::
  arg       ()
  ret int   (4, 0[1], {v | v >= 0})
  store_in  []
  store_out []

nondet ::
  arg       ()
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

dummyassert ::
  arg 	    (b: int(4, true, {v | true}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []


assert ::
  arg 	    (b: int(4, true, {v | v != 0}))
  ret int   (4, true, {v | true})
  store_in  []
  store_out []

csolve_exit ::
  arg       (status: int (4, true, {v | true}))
  ret int   (4, true, {v | 0=1})
  store_in  []
  store_out []

exit ::
  arg       (status: int (4, true, {v | true}))
  ret int   (0, true, {v | 0=1})
  store_in  []
  store_out []

atoi ::
  arg       (a: ref (A0, 0[1], {v | true}))
  ret       int (4, true, {v | true})
  store     [A0 |-> 0[1]: final int (1, true, {v | true})]

random ::
  arg       ()
  ret       int (4, true, {v | true})
  store_in  []
  store_out []

pow ::
  arg       (base: int (8, true), exp: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

clock ::
  arg       ()
  ret       int (4, true)
  store_in  []
  store_out []

sqrt ::
  arg       (n: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

fabs ::
  arg       (n: int (8, true))
  ret       int (8, true)
  store_in  []
  store_out []

bor ::
  arg       (a: int (4, true, {v | v >= 0}), b: int (4, true, {v | v >= 0}))
  ret       int (4, true, {v | && [a <= v; b <= v; v <= a + b]})
  store_in  []
  store_out []

band ::
  arg       (a: int (4, true), b: int (4, true, {v | v >= 0}))
  ret       int (4, true, {v | && [v <= a; v <= b; 0 <= v]})
  store_in  []
  store_out []

assume ::
  arg 	    (b: int(4, true))
  ret int   (4, true, {v | b = 1})
  store_in  []
  store_out []

read ::
  arg       (fd: int (4, true), dst: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

write ::
  arg       (fd: int (4, true), src: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

perror ::
  arg       (err: ref (A1, 0[1]))
  ret       int (0, true)
  store     [A1 |-> 0[1]: final int (1, true)]

strlen ::
  arg       (str: ref (A1, 0[1]))
  ret       int (4, 0[1])
  store     [A1 |-> 0[1]: final int (1, true)]
