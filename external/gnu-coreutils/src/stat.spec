// /usr/include/sys/stat.h:222: Error: No spec for extern function fstat. Autogen spec is:

fstat ::
  arg       (__fd : int(4, 0{1}),
             __buf : ref(A0, 0))
  ret       int(4, 0{1})
  store     [A0 |-> 0: int(8, 0{1}),
                    8: int(2, 0{1}),
                    12: int(4, 0{1}),
                    16: int(4, 0{1}),
                    20: int(4, 0{1}),
                    24: int(4, 0{1}),
                    28: int(4, 0{1}),
                    32: int(8, 0{1}),
                    40: int(2, 0{1}),
                    44: int(8, 0{1}),
                    52: int(4, 0{1}),
                    56: int(8, 0{1}),
                    64: int(4, 0{1}),
                    68: int(4, 0{1}),
                    72: int(4, 0{1}),
                    76: int(4, 0{1}),
                    80: int(4, 0{1}),
                    84: int(4, 0{1}),
                    88: int(8, 0{1})]
