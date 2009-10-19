type indextyping = (Ctypes.cfun * Ctypes.ctype CilMisc.VarMap.t) CilMisc.VarMap.t

val infer_indices: Ctypes.cfun CilMisc.VarMap.t -> Ssa_transform.ssaCfgInfo CilMisc.VarMap.t -> indextyping
