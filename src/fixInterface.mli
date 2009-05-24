type name
type cilenv
type reftype
type refldesc
type refstore


val name_of_varinfo: Cil.varinfo -> name
val ce_empty  : cilenv
val ce_adds   : cilenv -> (name * reftype) list -> cilenv
val ce_find   : name -> cilenv -> reftype
val ce_find_fn: name -> cilenv -> (name * reftype) list * reftype
val ce_project: cilenv -> cilenv -> name list -> cilenv
val print_ce  : Constraint.soln option -> Format.formatter -> cilenv -> unit




val t_fresh_typ : Cil.typ  -> reftype
val t_fresh     : Ctypes.ctype -> reftype
val t_true      : Ctypes.ctype -> reftype
val t_exp       : Ctypes.ctype -> Cil.exp -> reftype
val t_name      : cilenv -> name -> reftype
val t_subs_exps : (name * Cil.exp) list -> reftype -> reftype
val t_subs_names: (name * name) list -> reftype -> reftype



val make_cs: cilenv 
           -> Ast.pred 
           -> reftype 
           -> reftype 
           -> Cil.location 
           -> Constraint.t list

val make_wfs: cilenv 
           -> reftype 
           -> Cil.location 
           -> Constraint.wf list

val sorts: Ast.Sort.t list

val refstore_set: refstore -> Ctypes.sloc -> Ctypes.index -> reftype -> refstore
val refstore_get: refstore -> Ctypes.sloc -> Ctypes.index -> reftype
val refldesc_subs: refldesc -> (Ctypes.index -> reftype -> reftype) -> refldesc 
val ce_add_refldesc: cilenv -> Ctypes.sloc -> refldesc -> cilenv
