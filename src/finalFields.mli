val infer_final_fields :
  CilTag.o ->
  Ctypes.I.Spec.t ->
  (Ctypes.cfun * Ssa_transform.t * Ctypes.ctype CilMisc.VarMap.t) FixMisc.StringMap.t ->
  Shape.t FixMisc.StringMap.t ->
  Shape.t FixMisc.StringMap.t

val d_final_fields :
  unit ->
  Shape.final_fields_annot array ->
  Pretty.doc
