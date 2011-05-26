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

module Index:
  sig
    type class_bound = int option

    type bounded_congruence_class = {
      lb : class_bound;
      ub : class_bound;
      c  : int;
      m  : int;
    }

    type t =
      | IBot
      | IInt    of int
      | ICClass of bounded_congruence_class
    
    val top          : t
    val nonneg       : t
    val is_unbounded : t -> bool
    val period       : t -> int option
    val is_periodic  : t -> bool
    val of_int       : int -> t
    val mk_sequence  : int -> int -> class_bound -> class_bound -> t
    val mk_singleton : int -> t
    val mk_geq       : int -> t
    val mk_leq       : int -> t
    val mk_eq_mod    : int -> int -> t
    val lub          : t -> t -> t
    val glb          : t -> t -> t
    val widen        : t -> t -> t
    val offset       : int -> t -> t
    val plus         : t -> t -> t
    val minus        : t -> t -> t
    val scale        : int -> t -> t
    val mult         : t -> t -> t
    val div          : t -> t -> t
    val unsign       : t -> t
    val is_subindex  : t -> t -> bool
    val d_index      : unit -> t -> Pretty.doc
    val repr         : t -> string
    val repr_prefix  : string
  end

module IndexSet : Set.S with type elt = Index.t

val d_indexset : unit -> IndexSet.t -> Pretty.doc

(******************************************************************************)
(****************************** Type Refinements ******************************)
(******************************************************************************)

module type CTYPE_REFINEMENT = sig
  type t
  val is_subref    : t -> t -> bool
  val of_const     : Cil.constant -> t
  val top          : t
  val d_refinement : unit -> t -> Pretty.doc
end

module IndexRefinement: CTYPE_REFINEMENT with type t = Index.t

type finality =
  | Final
  | Nonfinal

type 'a prefield

type 'a preldesc

type 'a prestore

type 'a prectype =
  | Int of int * 'a         (* fixed-width integer *)
  | Ref of Sloc.t * 'a      (* reference *)

and 'a precfun =
    { qlocs       : Sloc.t list;                  (* generalized slocs *)
      args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
    }

type 'a prespec

module type S = sig
  module R : CTYPE_REFINEMENT

  module CType:
  sig
    type t = R.t prectype

    exception NoLUB of t * t

    val refinement       : t -> R.t
    val map              : ('a -> 'b) -> 'a prectype -> 'b prectype
    val d_ctype          : unit -> t -> Pretty.doc
    val of_const         : Cil.constant -> t
    val is_subctype      : t -> t -> bool
    val width            : t -> int
    val sloc             : t -> Sloc.t option
    val subs             : Sloc.Subst.t -> t -> t
    val eq               : t -> t -> bool
    val collide          : Index.t -> t -> Index.t -> t -> bool
    val is_void          : t -> bool
  end

  module Field:
  sig
    type t = R.t prefield

    val get_finality : t -> finality
    val set_finality : finality -> t -> t
    val is_final     : t -> bool
    val type_of      : t -> CType.t
    val sloc_of      : t -> Sloc.t option
    val create       : finality -> CType.t -> t
    val subs         : Sloc.Subst.t -> t -> t
    val map_type     : ('a prectype -> 'b prectype) -> 'a prefield -> 'b prefield

    val d_field      : unit -> t -> Pretty.doc
  end

  module LDesc:
  sig
    type t = R.t preldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    val empty         : t
    val add           : Cil.location -> Index.t -> Field.t -> t -> t
    val create        : Cil.location -> (Index.t * Field.t) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val referenced_slocs : t -> Sloc.SlocSet.t
    val find          : Index.t -> t -> (Index.t * Field.t) list
    val foldn         : (int -> 'a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val subs          : Sloc.Subst.t -> t -> t
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (Index.t -> Field.t -> unit) -> t -> unit
    val indices       : t -> Index.t list 
    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module Store:
  sig
    type t = R.t prestore

    val empty        : t
    val domain       : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val map          : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val find         : Sloc.t -> t -> LDesc.t
    val find_or_empty : Sloc.t -> t -> LDesc.t
    val fold_fields  : ('a -> Sloc.t -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val fold_data_locs : (Sloc.t -> LDesc.t -> 'a -> 'a) -> 'a -> t -> 'a
    val closed       : t -> bool
    val partition    : (Sloc.t -> bool) -> t -> t * t
    val add          : t -> Sloc.t -> LDesc.t -> t
    val remove       : t -> Sloc.t -> t
    val upd          : t -> t -> t
      (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
          overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : CType.t -> t -> bool
    val indices_of_t : t -> Index.t list 
    
    val d_store_addrs: unit -> t -> Pretty.doc
    val d_store      : unit -> t -> Pretty.doc
  end

  module CFun:
  sig
    type t = R.t precfun

    val d_cfun             : unit -> t -> Pretty.doc
    val map                : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val well_formed        : Store.t -> t -> bool
    val prune_unused_qlocs : t -> t
    val instantiate        : t -> t * (Sloc.t * Sloc.t) list
    val make               : Sloc.t list -> (string * CType.t) list -> CType.t -> Store.t -> Store.t -> t
    val subs               : Sloc.Subst.t -> t -> t
  end

  module Spec:
  sig
    type t = R.t prespec

    val empty   : t

    val map     : ('a -> 'b) -> 'a prespec -> 'b prespec
    val add_fun : bool -> string -> CFun.t * bool -> t -> t
    val add_var : bool -> string -> CType.t * bool -> t -> t
    val add_loc : Sloc.t -> LDesc.t -> t -> t
    val mem_fun : string -> t -> bool
    val mem_var : string -> t -> bool
    val get_fun : string -> t -> CFun.t * bool
    val get_var : string -> t -> CType.t * bool
    
    val store   : t -> Store.t
    val funspec : t -> (R.t precfun * bool) Misc.StringMap.t
    val varspec : t -> (R.t prectype * bool) Misc.StringMap.t

    val make    : (R.t precfun * bool) Misc.StringMap.t -> 
                  (R.t prectype * bool) Misc.StringMap.t -> 
                  Store.t -> 
                  t

    val add     : t -> t -> t
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
end

module Make (R: CTYPE_REFINEMENT) : S with module R = R

module I : S with module R = IndexRefinement

(******************************************************************************)
(*************************** Convenient Type Aliases **************************)
(******************************************************************************)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap

val void_ctype   : ctype 
val scalar_ctype : ctype

val d_ctype      : unit -> ctype -> Pretty.doc

(**********************************************************************)
(********************** refctypes and friends *************************)
(**********************************************************************)
module Reft      : CTYPE_REFINEMENT with type t = Index.t * FixConstraint.reft
module RefCTypes : S with module R = Reft

type refctype = RefCTypes.CType.t
type refcfun  = RefCTypes.CFun.t
type refldesc = RefCTypes.LDesc.t
type refstore = RefCTypes.Store.t
type refspec  = RefCTypes.Spec.t
type reffield = RefCTypes.Field.t

val d_refctype : unit -> refctype -> Pretty.doc
val d_refstore : unit -> refstore -> Pretty.doc
val d_refcfun  : unit -> refcfun -> Pretty.doc

val reft_of_top : FixConstraint.reft

val refstore_set        : refstore -> Sloc.t -> refldesc -> refstore
val refstore_get        : refstore -> Sloc.t -> refldesc
val refstore_fold       : (Sloc.t -> refldesc -> 'a -> 'a) -> 'a -> refstore -> 'a
val refstore_partition  : (Sloc.t -> bool) -> refstore -> refstore * refstore
val refstore_write      : Cil.location -> refstore -> refctype -> refctype -> refstore
val refstore_read       : Cil.location -> refstore -> refctype -> refctype


val refldesc_subs       : refldesc -> (int -> Index.t -> refctype -> refctype) -> refldesc

val is_soft_ptr         : Cil.location -> refstore -> refctype -> bool 
val addr_of_refctype    : Cil.location -> refctype -> Sloc.t * Index.t  
val index_of_ctype      : ctype -> Index.t

val ctype_of_refctype   : refctype -> ctype
val reft_of_refctype    : refctype -> FixConstraint.reft
val cfun_of_refcfun     : refcfun  -> cfun
val store_of_refstore   : refstore -> store
val cspec_of_refspec    : refspec  -> cspec
val qlocs_of_refcfun    : refcfun  -> Sloc.t list
val args_of_refcfun     : refcfun  -> (string * refctype) list
val ret_of_refcfun      : refcfun  -> refctype 
val stores_of_refcfun   : refcfun  -> refstore * refstore
val mk_refcfun          : Sloc.t list -> (string * refctype) list -> refstore -> refctype -> refstore -> refcfun 


