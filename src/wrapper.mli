type cilenv

val ce_empty: cilenv
val ce_add: Cil.varinfo -> Constraint.reft -> cilenv -> cilenv
val ce_find: Cil.varinfo -> cilenv -> (Cil.varinfo * Constraint.reft)
val ce_project: cilenv -> Cil.varinfo list -> cilenv
val ce_iter: (Cil.varinfo -> Constraint.reft -> unit) -> cilenv -> unit

val fresh: Cil.typ -> Constraint.reft
val t_single: Cil.typ -> Cil.exp -> Constraint.reft
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

type t
val empty_t: t
val add_t: t -> string -> Ssa_transform.ssaCfgInfo -> Constraint.wf list -> Constraint.t list -> cilenv -> t
val print_t: Constraint.soln option -> Format.formatter -> t ->  unit
val wfs_of_t: t -> Constraint.wf list 
val cs_of_t: t -> Constraint.t list
