type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k >= 0 *)

type sloc = int (* store locations *)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of sloc * 'a (* reference *)

type ctype = index prectype

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)

exception NoLUB of ctype * ctype

exception TypeDoesntFit

module SlocKey:
  sig
    type t = sloc
    val compare: t -> t -> int
  end

module SLM:
  sig
    type key = SlocKey.t
    type 'a t = 'a Map.Make(SlocKey).t
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

module LDesc:
  sig
    type 'a t
    val empty: 'a t
    val get_period: 'a t -> int option
    val collisions: ploc -> 'a prectype -> 'a t -> (ploc * 'a prectype) list
    val add: ploc -> 'a prectype -> 'a t -> 'a t
    val remove: ploc -> 'a t -> 'a t
    val shrink_period: int -> ('a prectype -> 'a prectype -> 'b -> 'b) -> 'b -> 'a t -> 'a t * 'b
    val find: ploc -> 'a t -> (ploc * 'a prectype) list
    val map: ('a prectype -> 'b prectype) -> 'a t -> 'b t
  end

type 'a prestore = ('a LDesc.t) SLM.t

type store = index prestore

(******************************************************************************)
(******************************* Pretty Printers ******************************)
(******************************************************************************)

val d_index: unit -> index -> Pretty.doc
val d_prectype: (unit -> 'a -> Pretty.doc) -> unit -> 'a prectype -> Pretty.doc
val d_ctype: unit -> ctype -> Pretty.doc
val d_store: unit -> store -> Pretty.doc

(******************************************************************************)
(****************************** Index Operations ******************************)
(******************************************************************************)

val index_lub: index -> index -> index
val index_plus: index -> index -> index
val index_scale: int -> index -> index
val is_subindex: index -> index -> bool

(******************************************************************************)
(******************************* Type Operations ******************************)
(******************************************************************************)

val prectype_width: 'a prectype -> int
val prectype_replace_sloc: sloc -> sloc -> 'a prectype -> 'a prectype
val ctype_lub: ctype -> ctype -> ctype
val is_subctype: ctype -> ctype -> bool

(******************************************************************************)
(************************ Periodic Location Operations ************************)
(******************************************************************************)

val ploc_start: ploc -> int
val ploc_compare: ploc -> ploc -> int
val ploc_le: ploc -> ploc -> bool
val ploc_periodic: ploc -> bool
val ploc_contains: ploc -> ploc -> int -> bool
val ploc_offset: ploc -> int -> ploc
val prectypes_collide: ploc -> 'a prectype -> ploc -> 'a prectype -> int -> bool

(******************************************************************************)
(****************************** Store Operations ******************************)
(******************************************************************************)

val prestore_find: sloc -> 'a prestore -> 'a LDesc.t
