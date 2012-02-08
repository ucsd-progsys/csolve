type class_bound = int option

type bounded_congruence_class = private {
  lb : class_bound;
  ub : class_bound;
  c  : int;
  m  : int;
}

type t =
  | IBot
  | IInt    of int
  | ICClass of bounded_congruence_class

type tIndex = t      
      
val top          : t
val ind_of_any   : t
val nonneg       : t
val is_unbounded : t -> bool
val period       : t -> int option
val is_periodic  : t -> bool
val of_int       : int -> t
val mk_sequence  : int -> int -> class_bound -> class_bound -> t
val mk_geq       : int -> t
val mk_leq       : int -> t
val mk_eq_mod    : int -> int -> t
val lub          : t -> t -> t
val glb          : t -> t -> t
val offset       : int -> t -> t
val plus         : t -> t -> t
val minus        : t -> t -> t
val scale        : int -> t -> t
val mult         : t -> t -> t
val div          : t -> t -> t
val ge           : t -> t
val gt           : t -> t
val le           : t -> t
val lt           : t -> t  
val unsign       : t -> t
val is_subindex  : t -> t -> bool
val overlaps     : t -> t -> bool
val d_index      : unit -> t -> Pretty.doc
val repr         : t -> string
val repr_prefix  : string

module IndexSet : Set.S with type elt = t

val d_indexset : unit -> IndexSet.t -> Pretty.doc

val index_of_reft      : FixConstraint.envt ->
                         (Ast.Symbol.t -> t) -> 
                         FixConstraint.reft ->
                         t
val ref_index_of_pred  : Ast.Symbol.t -> Ast.pred -> t
val data_index_of_pred : Ast.Symbol.t -> Ast.pred -> t
val apply_grd          : FixConstraint.envt -> Ast.pred -> FixConstraint.envt
