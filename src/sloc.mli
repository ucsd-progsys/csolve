type t

type sloctype = Abstract | Concrete

module SlocSet: Set.S with type elt = t
module SlocMap: Map.S with type key = t

val none          : t
val fresh         : sloctype -> t
val is_abstract   : t -> bool
val sloc_type     : t -> sloctype
val compare       : t -> t -> int
val eq            : t -> t -> bool
val unify         : t -> t -> unit
val to_string     : t -> string
val d_sloc        : unit -> t -> Pretty.doc

type subst = (t * t) list

val empty_subst   : subst
val subst_apply   : subst -> t -> t
val subst_extend  : t -> t -> subst -> subst
val subst_compose : subst -> subst -> subst
val subst_dom     : subst -> t list
val subst_rng     : subst -> t list
val subst_slocs   : subst -> t list
val d_subst       : unit -> subst -> Pretty.doc
