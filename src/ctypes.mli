(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

type seq_polarity = (* whether sequence extends positively only or in both directions *)
  | Pos
  | PosNeg

module Index:
  sig
    type t =
      | IBot                             (* empty sequence *)
      | IInt of int                      (* singleton n >= 0 *)
      | ISeq of int * int * seq_polarity (* arithmetic sequence (n, m): n + mk for all n, m >= 0, k *)

    val top         : t
    val nonneg      : t
    val of_int      : int -> t
    val lub         : t -> t -> t
    val plus        : t -> t -> t
    val minus       : t -> t -> t
    val scale       : int -> t -> t
    val mult        : t -> t -> t
    val div         : t -> t -> t
    val unsign      : t -> t
    val is_subindex : t -> t -> bool
    val d_index     : unit -> t -> Pretty.doc
  end

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of Sloc.t * 'a (* reference *)

type ctype = Index.t prectype

type ploc =
  | PLAt of int                 (* location n *)
  | PLSeq of int * seq_polarity (* location n plus periodic repeats *)

exception NoLUB of ctype * ctype

exception TypeDoesntFit

module LDesc:
  sig
    type 'a t
    val empty: 'a t
    val get_period: 'a t -> int option
    val add: ploc -> 'a prectype -> 'a t -> 'a t
    val add_index: Index.t -> 'a prectype -> 'a t -> 'a t
    val create: (Index.t * 'a prectype) list -> 'a t
    val remove: ploc -> 'a t -> 'a t
    val shrink_period: int -> ('a prectype -> 'a prectype -> 'b -> 'b) -> 'b -> 'a t -> 'a t * 'b
    val mem : ploc -> 'a t -> bool
    val find: ploc -> 'a t -> (ploc * 'a prectype) list
    val find_index: Index.t -> 'a t -> (ploc * 'a prectype) list
    val foldn: (int -> 'a -> ploc -> 'b prectype -> 'a) -> 'a -> 'b t -> 'a
    val fold: ('a -> ploc -> 'b prectype -> 'a) -> 'a -> 'b t -> 'a
    val map: ('a prectype -> 'b prectype) -> 'a t -> 'b t
    val mapn: (int -> ploc -> 'a prectype -> 'b prectype) -> 'a t -> 'b t
    val d_ldesc: (unit -> 'a prectype -> Pretty.doc) -> unit -> 'a t -> Pretty.doc
  end

module PreStore:
  sig
    type 'a t = ('a LDesc.t) Sloc.SlocMap.t

    val domain      : 'a t -> Sloc.t list
    val slocs       : 'a t -> Sloc.t list
    val map_ct      : ('a prectype -> 'b prectype) -> 'a t -> 'b t
    val map         : ('a -> 'b) -> 'a t -> 'b t
    val find        : Sloc.t -> 'a t -> 'a LDesc.t
    val find_index  : Sloc.t -> Index.t -> 'a t -> 'a prectype list
    val fold        : ('a -> Sloc.t -> Index.t -> 'b prectype -> 'a) -> 'a -> 'b t -> 'a
    val close_under : 'a t -> Sloc.t list -> 'a t
    val partition   : (Sloc.t -> 'a LDesc.t -> bool) -> 'a t -> ('a t * 'a t)
    val upd         : 'a t -> 'a t -> 'a t
      (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
          overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs        : Sloc.Subst.t -> 'a t -> 'a t

    val d_prestore_addrs : unit -> 'a t -> Pretty.doc
    val d_prestore       : (unit -> 'a -> Pretty.doc) -> unit -> 'a t -> Pretty.doc

    (* val prestore_split  : 'a prestore -> 'a prestore * 'a prestore
    (** [prestore_split sto] returns (asto, csto) s.t. 
       (1) sto = asto + csto
       (2) locs(asto) \in abslocs 
       (3) locs(csto) \in conlocs *)
       let prestore_split (ps: 'a prestore): 'a prestore * 'a prestore =
       prestore_partition (fun l _ -> S.is_abstract l) ps
    *)
  end

type store = Index.t PreStore.t

type 'a precfun =
  { qlocs       : Sloc.t list;                  (* generalized slocs *)
    args        : (string * 'a prectype) list;  (* arguments *)
    ret         : 'a prectype;                  (* return *)
    sto_in      : 'a PreStore.t;                (* in store *)
    sto_out     : 'a PreStore.t;                (* out store *)
  }

type cfun = Index.t precfun

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
val d_prectype: (unit -> 'a -> Pretty.doc) -> unit -> 'a prectype -> Pretty.doc
val d_precfun : (unit -> 'a -> Pretty.doc) -> unit -> 'a precfun -> Pretty.doc
val d_cfun    : unit -> cfun -> Pretty.doc
val d_ctype: unit -> ctype -> Pretty.doc
val d_store: unit -> store -> Pretty.doc
val d_ctemap: unit -> ctemap -> Pretty.doc

(******************************************************************************)
(****************************** Index Operations ******************************)
(******************************************************************************)

val ploc_of_index : Index.t -> ploc
val index_of_ploc : ploc -> int -> Index.t

(******************************************************************************)
(******************************* Type Operations ******************************)
(******************************************************************************)

val prectype_sloc: 'a prectype -> Sloc.t option
val prectype_map: ('a -> 'b) -> 'a prectype -> 'b prectype
val prectype_width: 'a prectype -> int
val prectype_subs : Sloc.Subst.t -> 'a prectype -> 'a prectype
val prectype_eq: 'a prectype -> 'a prectype -> bool
val ctype_lub: ctype -> ctype -> ctype
val is_subctype: ctype -> ctype -> bool
val ctype_of_const: Cil.constant -> ctype
val precfun_map: ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
val precfun_well_formed : 'a PreStore.t -> 'a precfun -> bool
val cfun_instantiate: 'a precfun -> 'a precfun * (Sloc.t * Sloc.t) list
val cfun_slocs : cfun -> Sloc.t list
val mk_cfun : Sloc.t list -> (string * 'a prectype) list -> 'a prectype -> 'a PreStore.t -> 'a PreStore.t -> 'a precfun
val cfun_subs : Sloc.Subst.t -> cfun -> cfun
val prectype_closed : 'a prectype -> 'a PreStore.t -> bool
val void_ctype: ctype
val is_void : 'a prectype -> bool
val is_ref : 'a prectype -> bool

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

val prestore_closed : 'a PreStore.t -> bool

(******************************************************************************)
(************************************ Specs ***********************************)
(******************************************************************************)

module PreSpec:
  sig
    type 'a t = ('a precfun * bool) Misc.StringMap.t * ('a prectype * bool) Misc.StringMap.t * 'a PreStore.t

    val empty: 'a t

    val map     : ('a -> 'b) -> 'a t -> 'b t
    val add_fun : string -> 'a precfun * bool -> 'a t -> 'a t
    val add_var : string -> 'a prectype * bool -> 'a t -> 'a t
    val add_loc : Sloc.t -> 'a LDesc.t -> 'a t -> 'a t
    val mem_fun : string -> 'a t -> bool
    val mem_var : string -> 'a t -> bool

    val store   : 'a t -> 'a PreStore.t
  end

type cspec = Index.t PreSpec.t
