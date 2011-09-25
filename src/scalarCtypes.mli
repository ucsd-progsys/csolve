

(*
type qualkind = Equality | Bound | Modulus | Other
*)

val scalar_quals_of_file : Cil.file -> Ast.Qualifier.t list  

val ref_index_of_pred    : Ast.Symbol.t -> Ast.pred -> Index.t
val data_index_of_pred   : Ast.Symbol.t -> Ast.pred -> Index.t
val index_of_var         : Cil.varinfo -> (Ctypes.refctype * Ast.pred) -> Index.t
val pred_of_index_int    : Index.t -> Ast.Symbol.t * Ast.pred
val pred_of_index_ref    : Index.t -> Ast.Symbol.t * Ast.pred
val pred_of_ctype        : Ctypes.ctype -> Ast.Symbol.t * Ast.pred
val partition_scalar_quals : Ast.Qualifier.t list 
                           -> ( Ast.Qualifier.t list 
                              * Ast.Qualifier.t list 
                              * Ast.Qualifier.t list)
