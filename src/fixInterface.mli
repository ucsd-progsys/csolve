type name
type cilenv
type reftype

val name_of_varinfo: Cil.varinfo -> name
val name_fresh: unit -> name

val ce_rem    : name -> cilenv -> cilenv 
val ce_mem    : name -> cilenv -> bool 
val ce_empty  : cilenv
val ce_adds   : cilenv -> (name * reftype) list -> cilenv
val ce_find   : name -> cilenv -> reftype
val ce_find_fn: name -> cilenv -> (name * reftype) list * reftype
val ce_project: cilenv -> cilenv -> name list -> cilenv
val print_ce  : Constraint.soln option -> Format.formatter -> cilenv -> unit

val t_fresh_typ         : Cil.typ  -> reftype
val t_fresh             : Ctypes.ctype -> reftype
val t_true              : Ctypes.ctype -> reftype
val t_true_reftype      : reftype -> reftype 
val t_exp               : Ctypes.ctype -> Cil.exp -> reftype
val t_name              : cilenv -> name -> reftype
val t_ctype_reftype     : Ctypes.ctype -> reftype -> reftype
val t_subs_exps         : (name * Cil.exp) list -> reftype -> reftype
val t_subs_names        : (name * name) list -> reftype -> reftype

type refldesc
type refstore
val refstore_empty      : refstore
val binds_of_refldesc   : Ctypes.sloc -> refldesc -> (name * reftype) list
val refstore_mem        : Ctypes.sloc -> refstore -> bool
val refstore_remove     : Ctypes.sloc -> refstore -> refstore
val refstore_set        : refstore -> Ctypes.sloc -> refldesc -> refstore
val refstore_get        : refstore -> Ctypes.sloc -> refldesc
val refldesc_subs       : refldesc -> (int -> reftype -> reftype) -> refldesc 
val refstore_write      : refstore -> reftype -> reftype -> refstore
val refstore_read       : refstore -> reftype -> reftype
val refstore_fresh      : Ctypes.store -> refstore


val sorts               : Ast.Sort.t list
val make_wfs            : cilenv -> reftype -> Cil.location -> Constraint.wf list
val make_wfs_refstore   : cilenv -> refstore -> Cil.location -> Constraint.wf list 
val make_cs             : cilenv -> Ast.pred -> reftype -> reftype -> Cil.location -> Constraint.t list
val make_cs_binds       : cilenv -> Ast.pred -> (name * reftype) list -> (name * reftype) list -> bool list -> Cil.location -> Constraint.t list


