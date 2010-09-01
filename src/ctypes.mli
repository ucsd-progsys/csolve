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

type ploc =
  | PLAt of int                 (* location n *)
  | PLSeq of int * seq_polarity (* location n plus periodic repeats *)

(******************************************************************************)
(****************************** Type Refinements ******************************)
(******************************************************************************)

module type CTYPE_REFINEMENT = sig
  type t

  val lub          : t -> t -> t option
  val is_subref    : t -> t -> bool
  val of_const     : Cil.constant -> t

  val d_refinement : unit -> t -> Pretty.doc
end

module IndexRefinement: CTYPE_REFINEMENT with type t = Index.t

type 'a prectype =
  | Int of int * 'a     (* fixed-width integer *)
  | Ref of Sloc.t * 'a  (* reference *)

type 'a preldesc

type 'a prestore = ('a preldesc) Sloc.SlocMap.t

type 'a precfun =
    { qlocs       : Sloc.t list;                  (* generalized slocs *)
      args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
    }

type 'a prespec = ('a precfun * bool) Misc.StringMap.t * ('a prectype * bool) Misc.StringMap.t * 'a prestore

(* can this be a functor? *)
module type S = sig
  module R : CTYPE_REFINEMENT

  module CType:
  sig
    type t = R.t prectype

    exception NoLUB of t * t

    val map         : ('a -> 'b) -> 'a prectype -> 'b prectype
    val d_ctype     : unit -> t -> Pretty.doc
    val of_const    : Cil.constant -> t
    val is_subctype : t -> t -> bool
    val width       : t -> int
    val sloc        : t -> Sloc.t option
    val subs        : Sloc.Subst.t -> t -> t
    val eq          : t -> t -> bool
    val collide     : ploc -> t -> ploc -> t -> int -> bool
    val is_void     : t -> bool
    val is_ref      : t -> bool
  end

  module LDesc:
  sig
    type t = R.t preldesc

    exception TypeDoesntFit of ploc * CType.t * t

    val empty         : t
    val get_period    : t -> int option
    val add           : Cil.location -> ploc -> CType.t -> t -> t
    val add_index     : Cil.location -> Index.t -> CType.t -> t -> t
    val create        : Cil.location -> (Index.t * CType.t) list -> t
    val remove        : ploc -> t -> t
    val shrink_period : int -> (CType.t -> CType.t -> 'b -> 'b) -> 'b -> t -> t * 'b
    val mem           : ploc -> t -> bool
    val find          : ploc -> t -> (ploc * CType.t) list
    val find_index    : Index.t -> t -> (ploc * CType.t) list
    val foldn         : (int -> 'a -> ploc -> CType.t -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> ploc -> CType.t -> 'a) -> 'a -> t -> 'a
    val map           : ('a prectype -> 'b prectype) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> ploc -> 'a prectype -> 'b prectype) -> 'a preldesc -> 'b preldesc
    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module Store:
  sig
    type t = R.t prestore

    val domain       : t -> Sloc.t list
    val slocs        : t -> Sloc.t list
    val map_ct       : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val map          : ('a -> 'b) -> 'a prestore -> 'b prestore
    val find         : Sloc.t -> t -> LDesc.t
    val find_index   : Sloc.t -> Index.t -> t -> CType.t list
    val fold         : ('a -> Sloc.t -> Index.t -> CType.t -> 'a) -> 'a -> t -> 'a
    val close_under  : t -> Sloc.t list -> t
    val closed       : t -> bool
    val partition    : (Sloc.t -> LDesc.t -> bool) -> t -> t * t
    val upd          : t -> t -> t
      (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
          overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : CType.t -> t -> bool

    val d_store_addrs : unit -> t -> Pretty.doc
    val d_store       : unit -> t -> Pretty.doc

    (* val prestore_split  : 'a prestore -> 'a prestore * 'a prestore
    (** [prestore_split sto] returns (asto, csto) s.t. 
       (1) sto = asto + csto
       (2) locs(asto) \in abslocs 
       (3) locs(csto) \in conlocs *)
       let prestore_split (ps: 'a prestore): 'a prestore * 'a prestore =
       prestore_partition (fun l _ -> S.is_abstract l) ps
    *)
  end

  module CFun:
  sig
    type t = R.t precfun

    val d_cfun             : unit -> t -> Pretty.doc
    val map                : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val well_formed        : Store.t -> t -> bool
    val prune_unused_qlocs : t -> t
    val instantiate        : t -> t * (Sloc.t * Sloc.t) list
    val slocs              : t -> Sloc.t list
    val make               : Sloc.t list -> (string * CType.t) list -> CType.t -> Store.t -> Store.t -> t
    val subs               : Sloc.Subst.t -> t -> t
  end

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

  type ctemap = CType.t ExpMap.t

  val d_ctemap: unit -> ctemap -> Pretty.doc

  module Spec:
  sig
    type t = R.t prespec

    val empty   : t

    val map     : ('a -> 'b) -> 'a prespec -> 'b prespec
    val add_fun : string -> CFun.t * bool -> t -> t
    val add_var : string -> CType.t * bool -> t -> t
    val add_loc : Sloc.t -> LDesc.t -> t -> t
    val mem_fun : string -> t -> bool
    val mem_var : string -> t -> bool

    val store   : t -> Store.t
  end
end

module Make (R: CTYPE_REFINEMENT) : S with module R = R

module I : S with module R = IndexRefinement

(******************************************************************************)
(******************************* Pretty Printers ******************************)
(******************************************************************************)

val d_ploc : unit -> ploc -> Pretty.doc

(******************************************************************************)
(****************************** Index Operations ******************************)
(******************************************************************************)

val ploc_of_index : Index.t -> ploc
val index_of_ploc : ploc -> int -> Index.t

(******************************************************************************)
(************************ Periodic Location Operations ************************)
(******************************************************************************)

val ploc_start: ploc -> int
val ploc_compare: ploc -> ploc -> int
val ploc_periodic: ploc -> bool
val ploc_contains: ploc -> ploc -> int -> bool
val ploc_offset: ploc -> int -> ploc

(******************************************************************************)
(*************************** Convenient Type Aliases **************************)
(******************************************************************************)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap

val void_ctype : I.CType.t
