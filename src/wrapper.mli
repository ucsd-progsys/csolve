type cilenv
type cilreft

val ce_empty: cilenv
val ce_add: Cil.varinfo -> cilreft -> cilenv -> cilenv
val ce_find: Cil.varinfo -> cilenv -> cilreft
val ce_project: cilenv -> Cil.varinfo list -> cilenv
(* val ce_iter: (Cil.varinfo -> cilreft -> unit) -> cilenv -> unit *)
val print_ce: Constraint.soln option -> Format.formatter -> cilenv -> unit


val t_fresh: Cil.typ -> cilreft
val t_true: Cil.typ -> cilreft
val t_single: Cil.typ -> Cil.exp -> cilreft
val t_var: Cil.varinfo -> cilreft

val make_ts: cilenv 
           -> Ast.pred 
           -> cilreft 
           -> cilreft 
           -> Cil.location 
           -> Constraint.t list

val make_wfs: cilenv 
           -> cilreft 
           -> Cil.location 
           -> Constraint.wf list

type t
val empty_t: t
val add_t  : t 
           -> string 
           -> Ssa_transform.ssaCfgInfo 
           -> Constraint.wf list 
           -> Constraint.t list 
           -> cilenv 
           -> t
val print_t:  Constraint.soln option 
           -> Format.formatter 
           -> t 
           ->  unit
val wfs_of_t: t -> Constraint.wf list 
val cs_of_t: t -> Constraint.t list
