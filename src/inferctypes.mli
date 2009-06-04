module ExpKey:
  sig
    type t = Cil.exp
    val compare: t -> t -> int
  end

module ExpMap:
  sig
    type key = ExpKey.t
    type 'a t = 'a Map.Make(ExpKey).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end

type ctemap = Ctypes.ctype ExpMap.t

type shape = (Cil.varinfo * Ctypes.ctype) list * ctemap * Ctypes.store (* * block_annotation array *)

type funmap = (Ctypes.cfun * Ssa_transform.ssaCfgInfo) Misc.StringMap.t

val d_ctemap: unit -> ctemap -> Pretty.doc
val d_vartypes: unit -> (Cil.varinfo * Ctypes.ctype) list -> Pretty.doc

val infer_sci_shapes: Ssa_transform.ssaCfgInfo -> (Cil.varinfo * Ctypes.ctype) list * ctemap * Ctypes.store
val infer_shapes: Ctypes.ctypeenv -> funmap -> shape Misc.StringMap.t
