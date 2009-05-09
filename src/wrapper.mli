type cilenv

val ce_empty: cilenv
val ce_add: Cil.varinfo -> Constraint.reft -> cilenv -> cilenv
val ce_find: Cil.varinfo -> cilenv -> (Cil.varinfo * Constraint.reft)
val ce_project: cilenv -> Cil.varinfo list -> cilenv

val fresh: Cil.typ -> Constraint.reft
val t_single: Cil.exp -> Constraint.reft
val t_var: Cil.varinfo -> Constraint.reft

val make_ts: cilenv 
           -> Ast.pred 
           -> Constraint.reft 
           -> Constraint.reft 
           -> Cil.location 
           -> Constraint.t list

val make_wfs: cilenv 
           -> Constraint.reft 
           -> Cil.location 
           -> Constraint.wf list
