type name
type cilenv
type refctype = (Ctypes.index * Constraint.reft) Ctypes.prectype
type refcfun  (*= (Ctypes.index * Constraint.reft) Ctypes.precfun *)
type refldesc (*= (Ctypes.index * Constraint.reft) Ctypes.precfun *)
type refstore = (Ctypes.index * Constraint.reft) Ctypes.prestore

val ctype_of_refctype   : refctype -> Ctypes.index Ctypes.prectype
val cfun_of_refcfun     : refcfun  -> Ctypes.index Ctypes.precfun
val args_of_refcfun     : refcfun  -> (name * refctype) list
val ret_of_refcfun      : refcfun  -> refctype 
val stores_of_refcfun   : refcfun  -> refstore * refstore
val mk_cfun             : Ctypes.sloc list -> (string * refctype) list -> refstore -> refctype -> refstore -> refcfun 

val name_of_string      : string -> name
val name_of_varinfo     : Cil.varinfo -> name
val name_fresh          : unit -> name

val ce_rem              : name -> cilenv -> cilenv 
val ce_mem              : name -> cilenv -> bool 
val ce_empty            : cilenv
val ce_adds             : cilenv -> (name * refctype) list -> cilenv
val ce_find             : name -> cilenv -> refctype
val ce_adds_fn          : cilenv -> (string * refcfun) list -> cilenv
val ce_find_fn          : string -> cilenv -> refcfun

val t_fresh_fn          : Ctypes.cfun  -> refcfun
val t_fresh             : Ctypes.ctype -> refctype
val t_true              : Ctypes.ctype -> refctype
val t_true_refctype     : refctype -> refctype
val t_pred              : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> refctype
val t_exp               : Ctypes.ctype -> Cil.exp -> refctype
val t_name              : cilenv -> name -> refctype
val t_ctype_refctype    : Ctypes.ctype -> refctype -> refctype

val t_subs_names        : (name * name) list -> refctype -> refctype
val t_subs_exps         : (name * Cil.exp) list -> refctype -> refctype 
val t_subs_locs         : (Ctypes.sloc * Ctypes.sloc) list -> refctype -> refctype 

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
val refstore_subs_exps  : (name * Cil.exp) list -> refstore -> refstore

val sorts               : Ast.Sort.t list

val make_wfs            : cilenv -> refctype -> Cil.location -> Constraint.wf list
val make_wfs_fn         : cilenv -> refcfun -> Cil.location -> Constraint.wf list
val make_wfs_refstore   : cilenv -> refstore -> Cil.location -> Constraint.wf list
val make_cs             : cilenv -> Ast.pred -> 
                          refctype -> refctype -> 
                          Cil.location -> Constraint.t list
val make_cs_refldesc    : cilenv -> Ast.pred -> 
                          (Ctypes.sloc * refldesc) -> (Ctypes.sloc * refldesc) -> 
                          Cil.location -> Constraint.t list
val make_cs_refstore    : cilenv -> Ast.pred -> 
                          refstore -> refstore -> bool ->
                          Cil.location -> Constraint.t list


