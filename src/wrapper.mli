type cilenv
type cilcstr

val ce_empty: cilenv
val ce_add: Cil.varinfo -> Constraint.reft -> cilenv -> cilenv
val ce_find: Cil.varinfo -> cilenv -> (Cil.varinfo * Constraint.reft)

val expr_of_var: Cil.varinfo -> Ast.expr
val expr_of_lval: Cil.lval -> Ast.expr
val expr_of_cilexp: Cil.exp -> Ast.expr

val fresh: Cil.typ -> Constraint.reft
val t_single: Cil.exp -> Constraint.reft
val t_var: Cil.varinfo -> Constraint.reft

val mk_cilcstr: cilenv -> (int * bool) list -> Constraint.reft -> Constraint.reft -> Cil.location -> cilcstr
val cstr_of_cilcstr:  Ssa_transform.ssaCfgInfo -> Ast.pred -> cilcstr -> Constraint.t

