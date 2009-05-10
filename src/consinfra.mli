type block = int
type t
val var_exp:    t -> Cil.varinfo -> Cil.exp
val location:   t -> block -> Cil.location
val ssa_srcs:   t -> block -> (Cil.varinfo * Cil.varinfo) list
val ssa_targs:  t -> block -> Cil.varinfo list
val reach_vars: t -> block -> Cil.varinfo list
val def_vars:   t -> block -> Cil.varinfo list
val create: Ssa_transform.ssaCfgInfo -> t


