type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton n >= 0 *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k, n, m >= 0 *)
  | ITop               (* sequence of all values (including negatives) *)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of Sloc.t * 'a (* reference *)

type ctype = index prectype

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)
  | PLEverywhere  (* location 0, plus repeats infinitely in both directions *)

exception NoLUB of ctype * ctype

exception TypeDoesntFit

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
    val find_index: index -> 'a t -> (ploc * 'a prectype) list
    val foldn: (int -> 'a -> ploc -> 'b prectype -> 'a) -> 'a -> 'b t -> 'a
    val map: ('a prectype -> 'b prectype) -> 'a t -> 'b t
    val mapn: (int -> ploc -> 'a prectype -> 'b prectype) -> 'a t -> 'b t
    val d_ldesc: (unit -> 'a prectype -> Pretty.doc) -> unit -> 'a t -> Pretty.doc
  end

type 'a prestore = ('a LDesc.t) Sloc.SlocMap.t

type store = index prestore

type 'a precfun =
  { qlocs       : Sloc.t list;                  (* generalized slocs *)
    args        : (string * 'a prectype) list;  (* arguments *)
    ret         : 'a prectype;                  (* return *)
    sto_in      : 'a prestore;                  (* in store *)
    sto_out     : 'a prestore;                  (* out store *)             
  }

type cfun = index precfun

type ctypeenv = cfun CilMisc.VarMap.t

module ExpKey:
  sig
    type t = Cil.exp
    val compare: t -> t -> int
  end

module ExpMap: Map.S with type key = ExpKey.t

module ExpMapPrinter:
  sig
    val d_map:
      ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
      string ->
      (unit -> ExpMap.key -> Pretty.doc) ->
      (unit -> 'a -> Pretty.doc) -> unit -> 'a ExpMap.t -> Pretty.doc
  end

type ctemap = ctype ExpMap.t

(******************************************************************************)
(******************************* Pretty Printers ******************************)
(******************************************************************************)

val d_ploc : unit -> ploc -> Pretty.doc
val d_index: unit -> index -> Pretty.doc
val d_prectype: (unit -> 'a -> Pretty.doc) -> unit -> 'a prectype -> Pretty.doc
val d_precfun : (unit -> 'a -> Pretty.doc) -> unit -> 'a precfun -> Pretty.doc
val d_cfun    : unit -> cfun -> Pretty.doc
val d_ctype: unit -> ctype -> Pretty.doc
val d_precstore : (unit -> 'a -> Pretty.doc) -> unit -> 'a prestore -> Pretty.doc
val d_store: unit -> store -> Pretty.doc
val d_prestore_addrs: unit -> 'a prestore -> Pretty.doc
val d_ctemap: unit -> ctemap -> Pretty.doc

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
val index_div: index -> index -> index
val is_subindex: index -> index -> bool

(******************************************************************************)
(********************************* Type Values ********************************)
(******************************************************************************)

val void_ctype: ctype

(******************************************************************************)
(******************************* Type Operations ******************************)
(******************************************************************************)

val prectype_sloc: 'a prectype -> Sloc.t option
val prectype_map: ('a -> 'b) -> 'a prectype -> 'b prectype
val prectype_width: 'a prectype -> int
val prectype_subs        : Sloc.Subst.t -> 'a prectype -> 'a prectype
val prectype_eq: 'a prectype -> 'a prectype -> bool
val ctype_lub: ctype -> ctype -> ctype
val is_subctype: ctype -> ctype -> bool
val precfun_map: ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
val cfun_well_formed     : cfun -> bool
val mk_cfun : Sloc.t list -> (string * 'a prectype) list -> 'a prectype -> 'a prestore -> 'a prestore -> 'a precfun
val cfun_subs : Sloc.Subst.t -> cfun -> cfun
val ctype_closed         : ctype -> store -> bool
val is_void : 'a prectype -> bool

(******************************************************************************)
(************************ Periodic Location Operations ************************)
(******************************************************************************)

val ploc_start: ploc -> int
val ploc_compare: ploc -> ploc -> int
val ploc_periodic: ploc -> bool
val ploc_contains: ploc -> ploc -> int -> bool
val ploc_offset: ploc -> int -> ploc
val prectypes_collide: ploc -> 'a prectype -> ploc -> 'a prectype -> int -> bool

(******************************************************************************)
(****************************** Store Operations ******************************)
(******************************************************************************)

val prestore_domain : 'a prestore -> Sloc.t list
val prestore_map_ct : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
val prestore_map    : ('a -> 'b) -> 'a prestore -> 'b prestore
val prestore_find   : Sloc.t -> 'a prestore -> 'a LDesc.t
val prestore_find_index : Sloc.t -> index -> 'a prestore -> 'a prectype list
val prestore_fold   : ('a -> Sloc.t -> index -> 'b prectype -> 'a) -> 'a -> 'b prestore -> 'a
val prestore_close_under : 'a prestore -> Sloc.t list -> 'a prestore

val prestore_split  : 'a prestore -> 'a prestore * 'a prestore
(** [prestore_split sto] returns (asto, csto) s.t. 
	(1) sto = asto + csto
	(2) locs(asto) \in abslocs 
	(3) locs(csto) \in conlocs *)

val prestore_upd    : 'a prestore -> 'a prestore -> 'a prestore
(** [prestore_upd st1 st2] returns the store obtained by overwriting the
    common locations of st1 and st2 with the blocks appearing in st2 *)

val prestore_subs   : Sloc.Subst.t -> 'a prestore -> 'a prestore

val store_closed : index prestore -> bool
