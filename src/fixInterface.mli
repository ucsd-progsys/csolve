type name
type cilenv
type cilreft

val name_of_varinfo: Cil.varinfo -> name
val ce_empty  : cilenv
val ce_adds   : cilenv -> (name * cilreft) list -> cilenv
val ce_find   : name -> cilenv -> cilreft
val ce_find_fn: name -> cilenv -> (name * cilreft) list * cilreft
val ce_project: cilenv -> cilenv -> name list -> cilenv
val print_ce  : Constraint.soln option -> Format.formatter -> cilenv -> unit

val t_fresh_typ : Cil.typ  -> cilreft
val t_fresh     : Ctypes.ctype -> cilreft
val t_true      : Ctypes.ctype -> cilreft
val t_exp       : Ctypes.ctype -> Cil.exp -> cilreft
val t_name      : cilenv -> name -> cilreft

val t_subs_exps : (name * Cil.exp) list -> cilreft -> cilreft
val t_subs_names: (name * name) list -> cilreft -> cilreft

val make_cs: cilenv 
           -> Ast.pred 
           -> cilreft 
           -> cilreft 
           -> Cil.location 
           -> Constraint.t list

val make_wfs: cilenv 
           -> cilreft 
           -> Cil.location 
           -> Constraint.wf list

val sorts: Ast.Sort.t list

