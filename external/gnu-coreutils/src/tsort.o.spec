// ../lib/long-options.h:20: Error: No spec for extern function parse_long_options. Autogen spec is:

parse_long_options ::
  arg       (_argc : int(4, 0{1}),
             _argv : ref(A168, 0),
             _command_name : ref(A170, 0),
             _package : ref(A170, 0),
             _version : ref(A170, 0),
             _usage : ref(A171, 0))
  ret       int(0, 0{1})
  store     [A168 |-> 0: ref(A169, 0);
             A169 |-> 0: int(1, 0{1});
             A170 |-> 0: int(1, 0{1});
             A171 |-> arg       (x0 : int(4, 0{1}))
                      ret       int(0, 0{1})
                      store     []]

// ../lib/readtokens.h:33: Error: No spec for extern function init_tokenbuffer. Autogen spec is:

init_tokenbuffer ::
  arg       (tokenbuffer : ref(A172, 0))
  ret       int(0, 0{1})
  store     [A172 |-> 0: int(4, 0{1}),
                      4: ref(A173, 0[1]);
             A173 |-> 0[1]: int(1, 0{1})]


// ../lib/stdio-safer.h:27: Error: No spec for extern function freopen_safer. Autogen spec is:

freopen_safer ::
  arg       (autoarg0 : ref(A179, 0[1]),
             autoarg1 : ref(A180, 0[1]),
             autoarg2 : ref(A181, 0))
  ret       ref(A181, 0)
  store     [A179 |-> 0[1]: int(1, 0{1});
             A180 |-> 0[1]: int(1, 0{1});
             A181 |-> 0: int(4, 0{1}),
                      4: ref(A182, 0),
                      8: ref(A182, 0),
                      12: ref(A182, 0),
                      16: ref(A182, 0),
                      20: ref(A182, 0),
                      24: ref(A182, 0),
                      28: ref(A182, 0),
                      32: ref(A182, 0),
                      36: ref(A182, 0),
                      40: ref(A182, 0),
                      44: ref(A182, 0),
                      48: ref(A183, 0),
                      52: ref(A181, 0),
                      56: int(4, 0{1}),
                      60: int(4, 0{1}),
                      64: int(4, 0{1}),
                      68: int(2, 0{1}),
                      70: int(1, 0{1}),
                      71: int(1, 0{1}),
                      72: ref(A184, 0),
                      76: int(8, 0{1}),
                      84: ref(A184, 0),
                      88: ref(A184, 0),
                      92: ref(A184, 0),
                      96: ref(A184, 0),
                      100: int(4, 0{1}),
                      104: int(4, 0{1}),
                      108[1 < 148]: int(1, 0{1});
             A182 |-> 0: int(1, 0{1});
             A183 |-> 0: ref(A183, 0),
                      4: ref(A181, 0),
                      8: int(4, 0{1});
             A184 |-> 0: int(0, 0{1})]

// JHALA: autogenerate below from shared-location-spec for head & zeros

loc A61 |-> 0: ref(A62, 0[1]),
            4: ref(A61, 0),
            8: ref(A61, 0),
            12: int(4, 0{1}),
            16: int(4, 0{1}),
            20: ref(A61, 0),
            24: ref(A63, 0)

loc A62 |-> 0[1]: int(1, 0{1})

loc A63 |-> 0: ref(A61, 0),
            4: ref(A63, 0)

loc A999 |-> 0: ref(A61, 0)

// zeros__lcc_heapify__ :: ref (A999, 0)
// head__lcc_heapify__  :: ref (A999, 0)

loc A99 |-> 0[1]: int(1, 0{1})
Version :: ref(A99, 0[1])


loc A98 |-> 0[1]: int(1, 0{1})
program_name :: ref(A98, 0[1])
