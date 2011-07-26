

val annot_var   : FixAstInterface.name -> Ctypes.refctype -> unit
val annot_fun   : string -> Ctypes.refcfun -> unit
val annot_sto   : string -> Ctypes.refstore -> unit
val clear       : 'a -> unit
val dump        : FixSolution.t -> unit
val stitch_shapes_ctypes : Cil.file -> Shape.t Misc.StringMap.t -> unit
