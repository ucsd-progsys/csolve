type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton n >= 0 *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k, n, m >= 0 *)
  | ITop               (* sequence of all values (including negatives) *)

type ptrkind =
  | Checked
  | Unchecked

type ('a, 'b) prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of Sloc.t * 'b * 'a (* reference *)

type ctype = (index, ptrkind) prectype

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)
  | PLEverywhere  (* location 0, plus repeats infinitely in both directions *)

exception TypeDoesntFit

module LDesc:
  sig
    type ('a, 'b) t
    val empty: ('a, 'b) t
    val get_period: ('a, 'b) t -> int option
    val add: ploc -> ('a, 'b) prectype -> ('a, 'b) t -> ('a, 'b) t
    val add_index: index -> ('a, 'b) prectype -> ('a, 'b) t -> ('a, 'b) t
    val create: (index * ('a, 'b) prectype) list -> ('a, 'b) t
    val remove: ploc -> ('a, 'b) t -> ('a, 'b) t
    val shrink_period: int -> (('a, 'b) prectype -> ('a, 'b) prectype -> 'c -> 'c) -> 'c -> ('a, 'b) t -> ('a, 'b) t * 'c
    val find: ploc -> ('a, 'b) t -> (ploc * ('a, 'b) prectype) list
    val find_index: index -> ('a, 'b) t -> (ploc * ('a, 'b) prectype) list
    val foldn: (int -> 'a -> ploc -> ('b, 'c) prectype -> 'a) -> 'a -> ('b, 'c) t -> 'a
    val fold: ('a -> ploc -> ('b, 'c) prectype -> 'a) -> 'a -> ('b, 'c) t -> 'a
    val map: (('a, 'b) prectype -> ('c, 'd) prectype) -> ('a, 'b) t -> ('c, 'd) t
    val mapn: (int -> ploc -> ('a, 'b) prectype -> ('c, 'd) prectype) -> ('a, 'b) t -> ('c, 'd) t
    val d_ldesc: (unit -> ('a, 'b) prectype -> Pretty.doc) -> unit -> ('a, 'b) t -> Pretty.doc
  end

type ('a, 'b) prestore = (('a, 'b) LDesc.t) Sloc.SlocMap.t

type store = (index, ptrkind) prestore

type ('a, 'b) precfun =
  { qlocs       : Sloc.t list;                        (* generalized slocs *)
    args        : (string * ('a, 'b) prectype) list;  (* arguments *)
    ret         : ('a, 'b) prectype;                  (* return *)
    sto_in      : ('a, 'b) prestore;                  (* in store *)
    sto_out     : ('a, 'b) prestore;                  (* out store *)
  }

type cfun = (index, ptrkind) precfun

type ctypeenv = cfun Misc.StringMap.t

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
val d_ptrkind: unit -> ptrkind -> Pretty.doc
val d_prectype: (unit -> 'a -> Pretty.doc) -> (unit -> 'b -> Pretty.doc) -> unit -> ('a, 'b) prectype -> Pretty.doc
val d_precfun : (unit -> 'a -> Pretty.doc) -> (unit -> 'b -> Pretty.doc) -> unit -> ('a, 'b) precfun -> Pretty.doc
val d_cfun    : unit -> cfun -> Pretty.doc
val d_ctype: unit -> ctype -> Pretty.doc
val d_precstore : (unit -> 'a -> Pretty.doc) -> (unit -> 'b -> Pretty.doc) -> unit -> ('a, 'b) prestore -> Pretty.doc
val d_store: unit -> store -> Pretty.doc
val d_prestore_addrs: unit -> ('a, 'b) prestore -> Pretty.doc
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
val index_unsign: index -> index
val is_subindex: index -> index -> bool

(******************************************************************************)
(*************************** Pointer Kind Operators ***************************)
(******************************************************************************)

val is_subptrkind: ptrkind -> ptrkind -> bool

(******************************************************************************)
(******************************* Type Operations ******************************)
(******************************************************************************)

val prectype_sloc: ('a, 'b) prectype -> Sloc.t option
val prectype_map: ('a -> 'b) -> ('a, 'c) prectype -> ('b, 'c) prectype
val prectype_width: ('a, 'b) prectype -> int
val prectype_subs : Sloc.Subst.t -> ('a, 'b) prectype -> ('a, 'b) prectype
val prectype_eq: ('a, 'b) prectype -> ('a, 'b) prectype -> bool
val is_subctype: ctype -> ctype -> bool
val ctype_of_const: Cil.constant -> ctype
val precfun_map: (('a, 'b) prectype -> ('c, 'd) prectype) -> ('a, 'b) precfun -> ('c, 'd) precfun
val cfun_instantiate: ('a, 'b) precfun -> ('a, 'b) precfun * (Sloc.t * Sloc.t) list
val cfun_well_formed     : cfun -> bool
val cfun_slocs : cfun -> Sloc.t list
val mk_cfun : Sloc.t list -> (string * ('a, 'b) prectype) list -> ('a, 'b) prectype -> ('a, 'b) prestore -> ('a, 'b) prestore -> ('a, 'b) precfun
val cfun_subs : Sloc.Subst.t -> cfun -> cfun
val ctype_closed         : ctype -> store -> bool
val void_ctype: ctype
val is_void : ('a, 'b) prectype -> bool

(******************************************************************************)
(************************ Periodic Location Operations ************************)
(******************************************************************************)

val ploc_start: ploc -> int
val ploc_compare: ploc -> ploc -> int
val ploc_periodic: ploc -> bool
val ploc_contains: ploc -> ploc -> int -> bool
val ploc_offset: ploc -> int -> ploc
val prectypes_collide: ploc -> ('a, 'b) prectype -> ploc -> ('a, 'b) prectype -> int -> bool

(******************************************************************************)
(****************************** Store Operations ******************************)
(******************************************************************************)

val prestore_domain : ('a, 'b) prestore -> Sloc.t list
val prestore_map_ct : (('a, 'b) prectype -> ('c, 'd) prectype) -> ('a, 'b) prestore -> ('c, 'd) prestore
val prestore_map    : ('a -> 'b) -> ('a, 'c) prestore -> ('b, 'c) prestore
val prestore_find   : Sloc.t -> ('a, 'b) prestore -> ('a, 'b) LDesc.t
val prestore_find_index : Sloc.t -> index -> ('a, 'b) prestore -> ('a, 'b) prectype list
val prestore_fold   : ('a -> Sloc.t -> index -> ('b, 'c) prectype -> 'a) -> 'a -> ('b, 'c) prestore -> 'a
val prestore_close_under : ('a, 'b) prestore -> Sloc.t list -> ('a, 'b) prestore

val prestore_split  : ('a, 'b) prestore -> ('a, 'b) prestore * ('a, 'b) prestore
(** [prestore_split sto] returns (asto, csto) s.t. 
	(1) sto = asto + csto
	(2) locs(asto) \in abslocs 
	(3) locs(csto) \in conlocs *)

val prestore_upd    : ('a, 'b) prestore -> ('a, 'b) prestore -> ('a, 'b) prestore
(** [prestore_upd st1 st2] returns the store obtained by overwriting the
    common locations of st1 and st2 with the blocks appearing in st2 *)

val prestore_subs   : Sloc.Subst.t -> ('a, 'b) prestore -> ('a, 'b) prestore

val store_closed : store -> bool
