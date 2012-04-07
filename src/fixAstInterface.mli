

type name = (* TBD: private *) Ast.Symbol.t 

val d_name : unit -> name -> Pretty.doc
val so_ref : Sloc.t -> Ast.Sort.t
val so_fref: Ast.Sort.t
val so_int: Ast.Sort.t
val vv_int : Ast.Symbol.t

(*
val so_skl: Ast.Sort.t
val so_bls: Ast.Sort.t
val so_pun: Ast.Sort.t
val vv_int : Ast.Symbol.t
val vv_int : Ast.Symbol.t
*)

val quals_of_file       : string -> Qualifier.t list 
val sorts               : Ast.Sort.t list
val axioms              : Ast.pred list
val builtinm            : FixConstraint.reft Ast.Symbol.SMap.t

val uf_bbegin           : Ast.Symbol.t

val eApp_bbegin         : Ast.expr -> Ast.expr 
val eApp_bend           : Ast.expr -> Ast.expr 
val eApp_uncheck        : Ast.expr -> Ast.expr 
val eApp_deref          : Ast.expr -> Ast.Sort.t -> Ast.expr 
val eApp_skolem         : Ast.expr -> Ast.expr 

val name_of_string      : string -> name
val name_of_varinfo     : Cil.varinfo -> name
val varinfo_of_name     : name -> Cil.varinfo option
val name_fresh          : unit -> name
val string_of_name      : name -> string
val base_of_name        : name -> name option

val maybe_deref         : Ast.expr -> Ast.expr option 

val sloc_of_string      : string -> Sloc.t
val string_of_sloc      : Sloc.t -> string 

val eff_read            : name
val eff_write           : name


module NameMap          : FixMisc.EMapType with type key = name 
