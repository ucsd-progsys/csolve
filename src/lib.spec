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

// pmr: Is there a better way of organizing these specs?

malloc ::
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0[A0], 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0[A0] |-> ]

//xmalloc: same spec as alloc, the (different) autogen spec is below

xmalloc ::
  arg (sz: int (4, true, {v | 0 <= v}))
  ret ref(C0[A0], 0, {v | && [0 < v; BLOCK_BEGIN([v]) = v; BLOCK_END([v]) = v + sz]})
  store_in []
  store_out [C0[A0] |-> ]

//xmalloc ::
//  arg       (s : int(4, 0{1}))
//  ret       ref(A164, 0)
//  store_in  []
//  store_out [A164 |-> 0: int(0, 0{1})]

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

// pmr: Technically the type of f is unsound, but the only place this is used is ks.c and we didn't check
// this originally. Must fix soon.
fgets ::
  arg   (s: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v ; v < BLOCK_END([v]); n <= (BLOCK_END([v]) - v)]}),
         n: int (4, true),
         f: ref(A1, 0))
  ret   ref (A0, 0[1], {v | (v != 0) => && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]})
  store [A0 |-> 0[1]: int (1, true);
         A1 |-> io_file]

getc_unlocked ::
        arg       (x0 : ref(A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) = 148]}))
        ret       int (4, 0{1}, {v | &&[(0-1) <= v; v < 256]})
        store     [A0 |-> io_file]

putchar_unlocked ::
        arg       (c: int (4, true))
        ret       int (4, true)
        store     []

ferror_unlocked ::
        arg       (x0 : ref(A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) = 148]}))
        ret       int (4, true)
        store     [A0 |-> io_file]

_IO_getc ::
        arg       (x0 : ref(A0, 0))
        ret       int (4, 0{1}, {v | &&[(0-1) <= v; v < 256]})
        store     [A0 |-> io_file]

_IO_putc ::
        arg       (c: int (4, true), x0 : ref(A0, 0))
        ret       int(4, true)
        store     [A0 |-> io_file]

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

// /usr/include/assert.h:71: 

__assert_fail ::
  arg       (__assertion : ref(A0, 0),
             __file : ref(A1, 0),
             __line : int(4, true, {v | 0=1}) ,
             __function : ref(A2, 0))
  ret       int(0, 0{1})
  store     [A0 |-> 0: int(1, 0{1});
             A1 |-> 0: int(1, 0{1});
             A2 |-> 0: int(1, 0{1})]

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
  arg       (a: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int (4, true, {v | true})
  store     [A0 |-> 0[1]: final int (1, true, {v | true})]

atol ::
  arg       (a: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int (4, true, {v | true})
  store     [A0 |-> 0[1]: final int (1, true)]

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
  ret       int (4, 0[1], {v | && [a <= v; b <= v; v <= a + b]})
  store_in  []
  store_out []

band ::
  arg       (a: int (4, true), b: int (4, true, {v | v >= 0}))
  ret       int (4, 0[1], {v | && [v <= a; v <= b; 0 <= v]})
  store_in  []
  store_out []

assume ::
  arg 	    (b: int(4, true))
  ret int   (4, true, {v | b = 1})
  store_in  []
  store_out []

// tighten this spec
read ::
  arg       (fd: int (4, true), dst: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

// tighten this spec
write ::
  arg       (fd: int (4, true), src: ref (A1, true), len: int (4, true))
  ret       int (4, true)
  store_in  [A1 |-> ]
  store_out [A1 |-> ]

perror ::
  arg       (err: ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int (0, true)
  store     [A1 |-> 0[1]: final int (1, true)]

strlen ::
  arg       (str: ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int (4, 0[1], {v | v >= 0})
  store     [A1 |-> 0[1]: final int (1, true)]

// pmr: Tighten this spec
fputs_unlocked ::
  arg       (__s : ref(A15, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
             __stream : ref(A1, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]}))
  ret       int(4, 0{1})
  store     [A15 |-> 0[1]: int(1, 0{1});
             A1  |-> io_file]

puts ::
  arg       (__s : ref(A0, 0[1]))
  ret       int(4, 0{1})
  store     [A0 |-> 0[1]: int(1, 0{1})]




strcmp ::
  arg       (__s1 : ref(A12, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
             __s2 : ref(A12, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int(4, 0{1})
  store     [A12 |-> 0[1]: int(1, 0{1})]

strncmp ::
  arg       (__s1 : ref(A12, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
             __s2 : ref(A12, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
             n: int (4, true))
  ret       int(4, 0{1})
  store     [A12 |-> 0[1]: int(1, 0{1})]

atexit ::
  arg       (f: ref (A1, 0, {v | v != 0}))
  ret       int (4, 0{1})
  store     [A1 |-> arg   ()
                    ret   int (0, 0{1})
                    store []]

fopen ::
  arg      (fname: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
            modes: ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       ref (A2, 0, {v | (v != 0) => && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) = 148]})
  store_in  [A0 |-> 0[1]: int (1, 0{1});
             A1 |-> 0[1]: int (1, 0{1})]
  store_out [A0 |-> 0[1]: int (1, 0{1});
             A1 |-> 0[1]: int (1, 0{1});
             A2 |-> io_file]

open ::
  arg   (f: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
         m: int (4, true))
  ret   int (4, true, {v | v >= (0-1)})
  store [A0 |-> 0[1]: int (1, 0{1})]

fclose ::
  arg    (f: ref (A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - BLOCK_BEGIN([v])) >= 148]}))
  ret    int (4, true)
  store  [A0 |-> io_file]

close ::
  arg    (f: int (4, true, {v | v >= 0}))
  ret    int (4, true, {v | || [v = 0; v = (0 - 1)]})
  store  []

setvbuf ::
  arg    (f: ref (A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - BLOCK_BEGIN([v])) >= 148]}),
          b: ref (A1, 0[1], {v | (v != 0) => && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - v) >= s]}),
          m: int (4, true),
          s: int (4, true, {v | v >= 0}))
  ret    int (4, true)
  store  [A0 |-> io_file;
          A1 |-> 0[1]: int (1, true)]

strtok ::
  arg      (str: ref (A0, 0[1], {v | (v != 0) => && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
            sep: ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret      ref (A0, 0[1], {v | (v != 0) => && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store    [A0 |-> 0[1]: int (1, 0{1});
            A1 |-> 0[1]: int (1, 0{1})]

// ../lib/readtokens.h:35: Error: No spec for extern function readtoken. Autogen spec is:

readtoken ::
  arg       (stream : ref(A0, 0),
             delim  : ref(A1, 0),
             n_delim : int(4, 0{1}),
             tokenbuffer : ref(A2, 0))
  ret       int(4, 0{1})
  store     [A0 |-> io_file;
             A1 |-> 0: int(1, 0{1});
             A2 |-> 0: int(4, 0{1}), 4: ref(A3, 0[1]);
             A3 |-> 0[1]: int(1, 0{1})]


umask ::
  arg      (m: int (4, true))
  ret      int (4, true)
  store    []

// Stuff from coreutils, dump elsewhere

last_component ::
  arg      (s: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret      ref (A0,
                0[1],
                {v | && [s <= v; v < BLOCK_END([v]); BLOCK_BEGIN([v]) = BLOCK_BEGIN([s]); BLOCK_END([v]) = BLOCK_END([s])]})
  store   [A0 |-> 0[1]: int (1, 0{1})]

setlocale ::
  arg       (i: int (4, 0{1}), s: ref (A0, 0[1], {v | (v != 0) => && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       ref (A0, 0[1], {v | (v != 0) => && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store     [A0 |-> 0[1]: int (1, 0{1})]

close_stdout ::
  arg   ()
  ret   int (0, true)
  store []

// pmr: Handling of this is almost surely unsound
quote ::
  arg       (s: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       ref (A1, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]})
  store_in  [A0 |-> 0[1]: int (1, true)]
  store_out [A0 |-> 0[1]: int (1, true);
             A1 |-> 0[1]: int (1, true)]

getopt_long ::
  arg       (argc: int (4, true, {v | v > 0}),
             argv: ref (A0, 0[1], {v | && [0 < v; v = BLOCK_BEGIN([v]); BLOCK_END([v]) >= (BLOCK_BEGIN([v]) + (4 * argc))]}),
             opts: ref (A2, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
             lopt: ref (A3, 0[1], {v | && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]}),
             lidx: ref (A6, 0, {v | (v != 0) => && [0 < v; BLOCK_BEGIN([v]) <= v; 4 <= (BLOCK_END([v]) - v)]}))
  ret       int (4, true)
  store     [A0 |-> 0[4]: ref(A1, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); v < BLOCK_END([v])]});
             A1 |-> 0[1]: int(1, true);
             A2 |-> 0[1]: int(1, true);
             A3 |-> 0[16]:  ref (A4, 0, {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
                    4[16]:  int (4, true),
                    8[16]:  ref (A5, 0, {v | (v != 0) => && [0 < v;
                                                             v = BLOCK_BEGIN([v]);
                                                             (BLOCK_END([v]) - BLOCK_BEGIN([v])) >= 4]}),
                    12[16]: int (4, true);
             A4 |-> 0[1]: int(1, true);
             A5 |-> 0: int(4, true);
             A6 |-> 0: int (4, true)]

// pmr: Really this is a global pointer - must fix
__errno_location ::
  arg       ()
  ret       ref (A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - BLOCK_BEGIN([v])) = 4]})
  store_in  []
  store_out [A0 |-> 0: int (4, true)]

// pmr: How to specify vararg functions, which we ignore anyway?

printf ::
  arg   ()
  ret   int (0, true)
  store []

fprintf ::
  arg   ()
  ret   int (0, true)
  store []

version_etc ::
  arg   ()
  ret   int (0, true)
  store []

prog_fprintf ::
  arg   ()
  ret   int (0, true)
  store []

error ::
  arg   ()
  ret   int (0, true)
  store []

gettext ::
  arg   (s: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret   ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store [A0 |-> 0[1]: int (1, true)]

emit_ancillary_info ::
  arg   ()
  ret   int (0, true)
  store []

bindtextdomain ::
  arg   (p: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
         d: ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret   ref (A1, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store [A0 |-> 0[1]: int (1, true);
         A1 |-> 0[1]: int (1, true)]

textdomain ::
  arg   (d: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret   ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store [A0 |-> 0[1]: int (1, true)]

// pmr: Not right - should use the bound defined in human.h as the minimum size of
// the buffer passed in
human_readable ::
  arg   (n: int (8, true),
         b: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}),
         o: int (4, true),
         f: int (8, true),
         t: int (8, true))
  ret   ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})
  store [A0 |-> 0[1]: int (1, true)]

safe_read ::
  arg   (f: int (4, true),
         b: ref (A0, 0[1], {v | && [0 < v; BLOCK_BEGIN([v]) <= v; (BLOCK_END([v]) - v) >= c]}),
         c: int (4, true, {v | v >= 0}))
  ret   int (4, true, {v | && [(0-1) <= v; v <= c]})
  store [A0 |-> 0[1]: int (1, true)]

fadvise ::
  arg    (f: ref (A0, 0, {v | && [0 < v; v = BLOCK_BEGIN([v]); (BLOCK_END([v]) - BLOCK_BEGIN([v])) >= 148]}),
          a: int (4, true))
  ret    int (0, true)
  store  [A0 |-> io_file]

optind :: int(4, 0{1})
