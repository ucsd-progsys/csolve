type cilenv
type cilcstr

val names_of_cilenv: cilenv -> string list

val ce_empty: cilenv
val ce_add: Cil.varinfo -> Constraint.reft -> cilenv -> cilenv
val ce_find: Cil.varinfo -> cilenv -> (Cil.varinfo * Constraint.reft)

val fresh: Cil.typ -> Constraint.reft
val t_single: Cil.exp -> Constraint.reft
val t_var: Cil.varinfo -> Constraint.reft

val mk_cilcstr: cilenv -> (int * bool) list -> Constraint.reft -> Constraint.reft -> Cil.location -> cilcstr
val cstr_of_cilcstr:  Ssa_transform.ssaCfgInfo -> Ast.pred -> cilcstr -> Constraint.t

