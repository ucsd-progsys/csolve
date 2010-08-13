module NASet : Set.S with type elt = Sloc.t * Sloc.t

val non_aliased_locations : Ssa.cfgInfo -> ShapeInfra.shape -> NASet.t list array
