loc A48 |-> 0: int(1, 0{1})

loc A49 |-> 0: int(4, 0{1})

x :: ref(A49, 0)

c :: ref(A48, 0)

one ::
  arg       (a : ref(A49, 0),
             b : ref(A48, 0))
  ret       int(0, 0{1})
  global    [A48; A49]
  store     []

two ::
  arg       (t : ref(A49, 0),
             u : ref(A48, 0))
  ret       int(0, 0{1})
  global    [A49; A48]
  store     []

