module NASet : Set.S with type elt = Sloc.t * Sloc.t

type block_annotation = NASet.t list

val d_naset               : unit -> NASet.t -> Pretty.doc
val non_aliased_locations : Ssa.cfgInfo -> Ctypes.ctemap -> (Refanno.cncm * Refanno.cncm) array -> Refanno.block_annotation array -> block_annotation array
