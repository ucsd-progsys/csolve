type name
type cilenv
type refctype  = (Ctypes.index * Constraint.reft) Ctypes.prectype
type refcfun   = (Ctypes.index * Constraint.reft) Ctypes.cfun

val name_of_varinfo: Cil.varinfo -> name
val name_fresh: unit -> name

val ce_rem    : name -> cilenv -> cilenv 
val ce_mem    : name -> cilenv -> bool 
val ce_empty  : cilenv
val ce_adds   : cilenv -> (name * refctype) list -> cilenv
val ce_find   : name -> cilenv -> refctype
val ce_adds_fn: cilenv -> (string * refcfun) list -> cilenv
val ce_find_fn: name -> cilenv -> refcfun
(* val ce_project: cilenv -> cilenv -> name list -> cilenv *)
val print_ce  : Constraint.soln option -> Format.formatter -> cilenv -> unit

val t_fresh_typ         : Cil.typ  -> refctype
val t_fresh             : Ctypes.ctype -> refctype
val t_true              : Ctypes.ctype -> refctype
val t_true_refctype       : refctype -> refctype
val t_pred              : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> refctype
val t_exp               : Ctypes.ctype -> Cil.exp -> refctype
val t_name              : cilenv -> name -> refctype
val t_ctype_refctype      : Ctypes.ctype -> refctype -> refctype
val t_subs_exps         : (name * Cil.exp) list -> refctype -> refctype
val t_subs_names        : (name * name) list -> refctype -> refctype

type refldesc
type refstore
val refstore_empty      : refstore
val binds_of_refldesc   : Ctypes.sloc -> refldesc -> (name * refctype) list
val refstore_mem        : Ctypes.sloc -> refstore -> bool
val refstore_remove     : Ctypes.sloc -> refstore -> refstore
val refstore_set        : refstore -> Ctypes.sloc -> refldesc -> refstore
val refstore_get        : refstore -> Ctypes.sloc -> refldesc
val refldesc_subs       : refldesc -> (int -> refctype -> refctype) -> refldesc 
val refstore_write      : refstore -> refctype -> refctype -> refstore
val refstore_read       : refstore -> refctype -> refctype
val refstore_fresh      : Ctypes.store -> refstore

val sorts               : Ast.Sort.t list

val make_wfs            : cilenv -> refctype -> Cil.location -> Constraint.wf list
val make_wfs_fn         : cilenv -> refcfun -> Cil.location -> Constraint.wf list
val make_wfs_refstore   : cilenv -> refstore -> Cil.location -> Constraint.wf list
val make_cs             : cilenv -> Ast.pred -> refctype -> refctype -> Cil.location -> Constraint.t list
val make_cs_binds       : cilenv -> Ast.pred -> (name * refctype) list -> (name * refctype) list -> bool list -> Cil.location -> Constraint.t list
