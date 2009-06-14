type t

type slocid = int

type sloctype = Abstract | Concrete

val create      : slocid -> sloctype -> t
val fresh       : sloctype -> t
val is_abstract : t -> bool
val repr        : t -> t
val sloc_type   : t -> sloctype
val compare     : t -> t -> int
val eq          : t -> t -> bool
val unify       : t -> t -> unit
val to_string   : t -> string
val d_sloc      : unit -> t -> Pretty.doc
