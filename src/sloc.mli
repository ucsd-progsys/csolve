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
val to_string     : t -> string
val d_sloc        : unit -> t -> Pretty.doc
val slm_bindings   : 'a SlocMap.t -> (t * 'a) list

type sloc = t

module Subst : sig
  type t = (sloc * sloc) list

  val empty        : t
  val apply        : t -> sloc -> sloc
  val extend       : sloc -> sloc -> t -> t
  val compose      : t -> t -> t
  val dom          : t -> sloc list
  val rng          : t -> sloc list
  val slocs        : t -> sloc list
  val well_defined : t -> bool
  val transpose    : t -> t
  val images       : t -> sloc list list
  val d_subst      : unit -> t -> Pretty.doc
end
