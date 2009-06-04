type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton n >= 0 *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k, n, m >= 0 *)
  | ITop               (* sequence of all values (including negatives) *)

type sloc = ALoc of int | CLoc of int   (* store location *)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of sloc * 'a (* reference *)

type ctype = index prectype

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)
  | PLEverywhere  (* location 0, plus repeats infinitely in both directions *)

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
    val add: ploc -> 'a prectype -> 'a t -> 'a t
    val create: (index * 'a prectype) list -> 'a t
    val remove: ploc -> 'a t -> 'a t
    val shrink_period: int -> ('a prectype -> 'a prectype -> 'b -> 'b) -> 'b -> 'a t -> 'a t * 'b
    val find: ploc -> 'a t -> (ploc * 'a prectype) list
    val foldn: (int -> 'a -> ploc -> 'b prectype -> 'a) -> 'a -> 'b t -> 'a
    val map: ('a prectype -> 'b prectype) -> 'a t -> 'b t
    val mapn: (int -> ploc -> 'a prectype -> 'b prectype) -> 'a t -> 'b t
    val d_ldesc: (unit -> 'a prectype -> Pretty.doc) -> unit -> 'a t -> Pretty.doc
  end

type 'a prestore = ('a LDesc.t) SLM.t

type store = index prestore

type 'a precfun =
  { qlocs       : sloc list;                    (* generalized slocs *)
    args        : (string * 'a prectype) list;  (* arguments *)
    ret         : 'a prectype;                  (* return *)
    sto_in      : 'a prestore;                  (* in store *)
    sto_out     : 'a prestore;                  (* out store *)             
  }

type cfun = index precfun

val precfun_map: ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
val d_precfun : (unit -> 'a -> Pretty.doc) -> unit -> 'a precfun -> Pretty.doc

(******************************************************************************)
(******************************* Pretty Printers ******************************)
(******************************************************************************)

val d_sloc: unit -> sloc -> Pretty.doc
val d_index: unit -> index -> Pretty.doc
val d_prectype: (unit -> 'a -> Pretty.doc) -> unit -> 'a prectype -> Pretty.doc
val d_ctype: unit -> ctype -> Pretty.doc
val d_store: unit -> store -> Pretty.doc

(******************************************************************************)
(****************************** Index Operations ******************************)
(******************************************************************************)

val index_of_int: int -> index
val index_of_ploc: ploc -> int -> index
val ploc_of_index: index -> ploc
val index_lub: index -> index -> index
val index_plus: index -> index -> index
val index_minus: index -> index -> index
val index_scale: int -> index -> index
val index_mult: index -> index -> index
val is_subindex: index -> index -> bool

(******************************************************************************)
(******************************* Type Operations ******************************)
(******************************************************************************)

val prectype_map: ('a -> 'b) -> 'a prectype -> 'b prectype
val prectype_width: 'a prectype -> int
val prectype_replace_sloc: sloc -> sloc -> 'a prectype -> 'a prectype
val ctype_lub: ctype -> ctype -> ctype
val is_subctype: ctype -> ctype -> bool
val cfun_instantiate: 'a precfun -> 'a precfun * (sloc * sloc) list

(******************************************************************************)
(************************** Store Location Operations *************************)
(******************************************************************************)

val fresh_sloc: unit -> sloc
val reset_fresh_slocs: unit -> unit

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

val prestore_map_ct : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
val prestore_map    : ('a -> 'b) -> 'a prestore -> 'b prestore
val prestore_find   : sloc -> 'a prestore -> 'a LDesc.t


val prestore_split  : 'a prestore -> 'a prestore * 'a prestore
(** [prestore_split sto] returns (asto, csto) s.t. 
	(1) sto = asto + csto
	(2) locs(asto) \in abslocs 
	(3) locs(csto) \in conlocs *)

val prestore_upd    : 'a prestore -> 'a prestore -> 'a prestore
(** [prestore_upd st1 st2] returns the store obtained by overwriting the
    common locations of st1 and st2 with the blocks appearing in st2 *)

val prestore_subs   : (sloc * sloc) list -> 'a prestore ->  'a prestore
val prectype_subs   : (sloc * sloc) list -> 'a prectype ->  'a prectype
