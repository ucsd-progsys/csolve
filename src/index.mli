type class_bound = int option

type bounded_congruence_class = {
  lb : class_bound;
  ub : class_bound;
  c  : int;
  m  : int;
}

type t =
  | IBot
  | IInt    of int
  | ICClass of bounded_congruence_class
      
val top          : t
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
val widen        : t -> t -> t
val offset       : int -> t -> t
val plus         : t -> t -> t
val minus        : t -> t -> t
val scale        : int -> t -> t
val mult         : t -> t -> t
val div          : t -> t -> t
val unsign       : t -> t
val is_subindex  : t -> t -> bool
val overlaps     : t -> t -> bool
val d_index      : unit -> t -> Pretty.doc
val repr         : t -> string
val repr_prefix  : string

module IndexSet : Set.S with type elt = t

val d_indexset : unit -> IndexSet.t -> Pretty.doc

module AbstractDomain : Config.DOMAIN
