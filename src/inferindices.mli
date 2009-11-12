type indextyping = (Ctypes.cfun * Ctypes.ctype CilMisc.VarMap.t) CilMisc.VarMap.t

type dcheck = Cil.varinfo * FixInterface.refctype

val d_dcheck          : unit -> dcheck -> Pretty.doc

val infer_fun_indices :
  Ctypes.cfun CilMisc.VarMap.t ->
  Ctypes.ctype CilMisc.VarMap.t ->
  Ssa_transform.ssaCfgInfo CilMisc.VarMap.t ->
  Ctypes.cfun ->
  Ssa_transform.ssaCfgInfo ->
  Ctypes.ctype CilMisc.VarMap.t * dcheck list
