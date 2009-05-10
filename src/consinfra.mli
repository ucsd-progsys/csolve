type t
val var_exp:    t -> Cil.varinfo -> Cil.exp
val location:   t -> int -> Cil.location
val ssa_srcs:   t -> int -> (Cil.varinfo * Cil.varinfo) list
val ssa_targs:  t -> int -> Cil.varinfo list
val reach_vars: t -> int -> Cil.varinfo list
val def_vars:   t -> int -> Cil.varinfo list
val guardp:     t -> int -> Ast.pred
val create: Ssa_transform.ssaCfgInfo -> t


