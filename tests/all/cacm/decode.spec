struct io_file (A16) =
  [A16 |->
      0: int(4, 0{1}),
      4: ref(A17, 0),
      8: ref(A17, 0),
      12: ref(A17, 0),
      16: ref(A17, 0),
      20: ref(A17, 0),
      24: ref(A17, 0),
      28: ref(A17, 0),
      32: ref(A17, 0),
      36: ref(A17, 0),
      40: ref(A17, 0),
      44: ref(A17, 0),
      48: ref(A18, 0),
      52: ref(A16, 0),
      56: int(4, 0{1}),
      60: int(4, 0{1}),
      64: int(4, 0{1}),
      68: int(2, 0{1}),
      70: int(1, 0{1}),
      71: int(1, 0{1}),
      72: ref(A19, 0),
      76: int(8, 0{1}),
      84: ref(A19, 0),
      88: ref(A19, 0),
      92: ref(A19, 0),
      96: ref(A19, 0),
      100: int(4, 0{1}),
      104: int(4, 0{1}),
      108[1 < 148]: int(1, 0{1});
  A17 |-> 0: int(1, 0{1});
  A18 |-> 0: ref(A18, 0),
          4: ref(A16, 0),
          8: int(4, 0{1});
  A19 |-> 0: int(0, 0{1})]

loc A1729 |-> io_file

stdin <: ref (A1729, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) = 148]})

loc A1730 |-> io_file

stdout <: ref (A1729, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) = 148]})