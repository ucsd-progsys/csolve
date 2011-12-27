

(*
type qualkind = Equality | Bound | Modulus | Other
*)

val index_of_var         : Cil.varinfo -> (Ctypes.refctype * Ast.pred) -> Index.t
  
val pred_of_index_int          : Index.t -> Ast.Symbol.t * Ast.pred
val pred_of_index_ref          : Index.t -> Ast.Symbol.t * Ast.pred
val non_null_pred_of_index_ref : Index.t -> Ast.Symbol.t * Ast.pred
val pred_of_ctype              : Ctypes.ctype -> Ast.Symbol.t * Ast.pred
val non_null_pred_of_ctype     : Ctypes.ctype -> Ast.Symbol.t * Ast.pred
val partition_scalar_quals     : Qualifier.t list -> ( Qualifier.t list * Qualifier.t list * Qualifier.t list)
