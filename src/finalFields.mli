val infer_final_fields :
  Ctypes.I.Spec.t ->
  (Ctypes.cfun * Ssa_transform.ssaCfgInfo * Ctypes.ctype CilMisc.VarMap.t) Misc.StringMap.t ->
  Shape.t Misc.StringMap.t ->
  Shape.t Misc.StringMap.t

val d_final_fields :
  unit ->
  Shape.final_fields_annot array ->
  Pretty.doc
