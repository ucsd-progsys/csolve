

val annot_var   : FixAstInterface.name -> Ctypes.refctype -> unit
val annot_fun   : string -> Ctypes.refcfun -> unit
val annot_sto   : string -> Ctypes.refstore -> unit
val annot_clear : 'a -> unit
val annot_dump  : FixSolution.t -> unit
