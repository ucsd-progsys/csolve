type ctemap = Ctypes.ctype Ctypes.ExpMap.t

type shape = (Cil.varinfo * Ctypes.ctype) list * ctemap * Ctypes.store (* * block_annotation array *)

type funmap = (Ctypes.cfun * Ssa_transform.ssaCfgInfo) Misc.StringMap.t

val d_ctemap: unit -> ctemap -> Pretty.doc
val d_vartypes: unit -> (Cil.varinfo * Ctypes.ctype) list -> Pretty.doc

val infer_sci_shapes: Ssa_transform.ssaCfgInfo -> (Cil.varinfo * Ctypes.ctype) list * ctemap * Ctypes.store
val infer_shapes: Ctypes.ctypeenv -> funmap -> shape Misc.StringMap.t
