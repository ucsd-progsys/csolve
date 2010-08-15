module NASet : Set.S with type elt = Sloc.t * Sloc.t

val d_naset               : unit -> NASet.t -> Pretty.doc
val non_aliased_locations : Ssa.cfgInfo -> ShapeInfra.shape -> NASet.t list array
