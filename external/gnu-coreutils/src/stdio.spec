loc A1729 |-> io_file
loc A1730 |-> io_file
loc A1731 |-> io_file

stdout <: ref (A1729, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = BLOCK_BEGIN([v]) + 148]})
stderr <: ref (A1730, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = BLOCK_BEGIN([v]) + 148]})
stdin  <: ref (A1731, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) = BLOCK_BEGIN([v]) + 148]})

// /usr/include/stdio.h:361: Error: No spec for extern function sprintf. Autogen spec is:

sprintf ::
  arg       (__s : ref(A201, 0),
             __format : ref(A202, 0))
  ret       int(4, 0{1})
  store     [A201 |-> 0: int(1, 0{1});
             A202 |-> 0: int(1, 0{1})]
// /usr/include/stdio.h:533: Error: No spec for extern function fgetc. Autogen spec is:

fgetc ::
  arg       (__stream : ref(A203, 0))
  ret       int(4, 0{1})
  store     [A203 |-> 0: int(4, 0{1}),
                      4: ref(A204, 0),
                      8: ref(A204, 0),
                      12: ref(A204, 0),
                      16: ref(A204, 0),
                      20: ref(A204, 0),
                      24: ref(A204, 0),
                      28: ref(A204, 0),
                      32: ref(A204, 0),
                      36: ref(A204, 0),
                      40: ref(A204, 0),
                      44: ref(A204, 0),
                      48: ref(A205, 0),
                      52: ref(A203, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A206, 0),
                      76: int(8, 0{1}),
                      84: ref(A206, 0),
                      88: ref(A206, 0),
                      92: ref(A206, 0),
                      96: ref(A206, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A204 |-> 0: int(1, 0{1});
             A205 |-> 0: ref(A205, 0),
                      4: ref(A203, 0),
                      8: int(4, 0{1});
             A206 |-> 0: int(0, 0{1})]

// /usr/include/stdio.h:604: Error: No spec for extern function putc_unlocked. Autogen spec is:

putc_unlocked ::
  arg       (__c : int(4, 0{1}),
             __stream : ref(A207, 0))
  ret       int(4, 0{1})
  store     [A207 |-> 0: int(4, 0{1}),
                      4: ref(A208, 0),
                      8: ref(A208, 0),
                      12: ref(A208, 0),
                      16: ref(A208, 0),
                      20: ref(A208, 0),
                      24: ref(A208, 0),
                      28: ref(A208, 0),
                      32: ref(A208, 0),
                      36: ref(A208, 0),
                      40: ref(A208, 0),
                      44: ref(A208, 0),
                      48: ref(A209, 0),
                      52: ref(A207, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A210, 0),
                      76: int(8, 0{1}),
                      84: ref(A210, 0),
                      88: ref(A210, 0),
                      92: ref(A210, 0),
                      96: ref(A210, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A208 |-> 0: int(1, 0{1});
             A209 |-> 0: ref(A209, 0),
                      4: ref(A207, 0),
                      8: int(4, 0{1});
             A210 |-> 0: int(0, 0{1})]

// /usr/include/stdio.h:730: Error: No spec for extern function fread_unlocked. Autogen spec is:

fread_unlocked ::
  arg       (__ptr : ref(A211, 0),
             __size : int(4, 0{1}),
             __n : int(4, 0{1}),
             __stream : ref(A212, 0))
  ret       int(4, 0{1})
  store     [A211 |-> 0: int(0, 0{1});
             A212 |-> 0: int(4, 0{1}),
                      4: ref(A213, 0),
                      8: ref(A213, 0),
                      12: ref(A213, 0),
                      16: ref(A213, 0),
                      20: ref(A213, 0),
                      24: ref(A213, 0),
                      28: ref(A213, 0),
                      32: ref(A213, 0),
                      36: ref(A213, 0),
                      40: ref(A213, 0),
                      44: ref(A213, 0),
                      48: ref(A214, 0),
                      52: ref(A212, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A211, 0),
                      76: int(8, 0{1}),
                      84: ref(A211, 0),
                      88: ref(A211, 0),
                      92: ref(A211, 0),
                      96: ref(A211, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A213 |-> 0: int(1, 0{1});
             A214 |-> 0: ref(A214, 0),
                      4: ref(A212, 0),
                      8: int(4, 0{1})]

// /usr/include/stdio.h:851: Error: No spec for extern function fileno. Autogen spec is:

fileno ::
  arg       (__stream : ref(A215, 0))
  ret       int(4, 0{1})
  store     [A215 |-> 0: int(4, 0{1}),
                      4: ref(A216, 0),
                      8: ref(A216, 0),
                      12: ref(A216, 0),
                      16: ref(A216, 0),
                      20: ref(A216, 0),
                      24: ref(A216, 0),
                      28: ref(A216, 0),
                      32: ref(A216, 0),
                      36: ref(A216, 0),
                      40: ref(A216, 0),
                      44: ref(A216, 0),
                      48: ref(A217, 0),
                      52: ref(A215, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A218, 0),
                      76: int(8, 0{1}),
                      84: ref(A218, 0),
                      88: ref(A218, 0),
                      92: ref(A218, 0),
                      96: ref(A218, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A216 |-> 0: int(1, 0{1});
             A217 |-> 0: ref(A217, 0),
                      4: ref(A215, 0),
                      8: int(4, 0{1});
             A218 |-> 0: int(0, 0{1})]

// ../lib/stdio.h:668: Error: No spec for extern function rpl_fseeko. Autogen spec is:

rpl_fseeko ::
  arg       (fp : ref(A219, 0),
             offset : int(8, 0{1}),
             whence : int(4, 0{1}))
  ret       int(4, 0{1})
  store     [A219 |-> 0: int(4, 0{1}),
                      4: ref(A220, 0),
                      8: ref(A220, 0),
                      12: ref(A220, 0),
                      16: ref(A220, 0),
                      20: ref(A220, 0),
                      24: ref(A220, 0),
                      28: ref(A220, 0),
                      32: ref(A220, 0),
                      36: ref(A220, 0),
                      40: ref(A220, 0),
                      44: ref(A220, 0),
                      48: ref(A221, 0),
                      52: ref(A219, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A222, 0),
                      76: int(8, 0{1}),
                      84: ref(A222, 0),
                      88: ref(A222, 0),
                      92: ref(A222, 0),
                      96: ref(A222, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A220 |-> 0: int(1, 0{1});
             A221 |-> 0: ref(A221, 0),
                      4: ref(A219, 0),
                      8: int(4, 0{1});
             A222 |-> 0: int(0, 0{1})]


