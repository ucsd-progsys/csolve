type indextyping = (Ctypes.cfun * Ctypes.ctype CilMisc.VarMap.t) CilMisc.VarMap.t

val infer_indices     : Ctypes.cfun CilMisc.VarMap.t -> Ssa_transform.ssaCfgInfo CilMisc.VarMap.t -> indextyping
val infer_fun_indices : Ctypes.cfun CilMisc.VarMap.t -> Ssa_transform.ssaCfgInfo CilMisc.VarMap.t -> Ctypes.cfun -> Ssa_transform.ssaCfgInfo -> Ctypes.ctype CilMisc.VarMap.t
