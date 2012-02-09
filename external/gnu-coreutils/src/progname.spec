set_program_name :: 
  arg       (argv0 : ref(A8, 0, {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]}))
  ret       int(0, 0{1})
  store     [A8 |-> 0[1]: int(1, 0{1})]


// ../lib/progname.h:32: Error: No spec for extern variable program_name. Autogen spec is:

loc A239 |-> 0: int(1, 0{1})

program_name <: ref(A239, 0, {v | && [0 < v; BLOCK_BEGIN([v]) <= v; v < BLOCK_END([v])]})


