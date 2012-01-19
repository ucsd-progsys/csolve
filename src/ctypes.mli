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

module IndexRefinement : CTYPE_REFINEMENT with type t = Index.t
module Reft            : CTYPE_REFINEMENT with type t = Index.t * FixConstraint.reft

type fieldinfo  = {fname : string option; ftype : Cil.typ option} 
type structinfo = {stype : Cil.typ option} 

val dummy_fieldinfo  : fieldinfo
val dummy_structinfo : structinfo
val d_fieldinfo      : unit -> fieldinfo -> Pretty.doc
val d_structinfo     : unit -> structinfo -> Pretty.doc

type finality =
  | Final
  | Nonfinal

type 'a prefield

type 'a preldesc

type 'a prestore

type 'a prectype =
  | Int of int * 'a         (* fixed-width integer *)
  | Ref of Sloc.t * 'a      (* reference *)
  | FRef of ('a precfun) * 'a  (* function reference *)

and  effectptr  = Reft.t prectype

and  effectset

and  'a precfun =
    { args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      globlocs    : Sloc.t list;                  (* unquantified locations *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
      effects     : effectset;                    (* heap effects *)
    }

type specType =
  | HasShape
  | IsSubtype
  | HasType

type 'a prespec

module EffectSet:
sig
  type t = effectset

  val empty       : t

  val apply       : (effectptr -> effectptr) -> effectset -> effectset
  val maplisti    : (Sloc.t -> effectptr -> 'a) -> effectset -> 'a list

  val find        : effectset -> Sloc.t -> effectptr
  val mem         : effectset -> Sloc.t -> bool
  val add         : effectset -> Sloc.t -> effectptr -> effectset

  val domain      : t -> Sloc.t list

  val d_effectset : unit -> effectset -> Pretty.doc
end

module type CTYPE_DEFS = sig
  module R : CTYPE_REFINEMENT

  type refinement = R.t

  type ctype = refinement prectype
  type field = refinement prefield
  type ldesc = refinement preldesc
  type store = refinement prestore
  type cfun  = refinement precfun
  type spec  = refinement prespec
end

module type S = sig
  module T : CTYPE_DEFS

  module CType:
  sig
    type t = T.ctype

    exception NoLUB of t * t

    val refinement       : t -> T.R.t
    val map              : ('a -> 'b) -> 'a prectype -> 'b prectype
    val map_func         : ('a -> 'b) -> 'a precfun -> 'b precfun
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
    type t = T.field

    val get_finality  : t -> finality
    val set_finality  : t -> finality -> t
    val get_fieldinfo : t -> fieldinfo
    val set_fieldinfo : t -> fieldinfo -> t

    val is_final     : t -> bool
    val type_of      : t -> CType.t
    val sloc_of      : t -> Sloc.t option
    val create       : finality -> fieldinfo -> CType.t -> t
    val subs         : Sloc.Subst.t -> t -> t
    val map_type     : ('a prectype -> 'b prectype) -> 'a prefield -> 'b prefield
    val d_field      : unit -> t -> Pretty.doc
  end

  module LDesc:
  sig
    type t = T.ldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    val empty         : t
    val eq            : t -> t -> bool
    val is_empty      : t -> bool
    val is_read_only  : t -> bool
    val add           : Index.t -> Field.t -> t -> t
    val create        : structinfo -> (Index.t * Field.t) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val referenced_slocs : t -> Sloc.t list
    val find          : Index.t -> t -> (Index.t * Field.t) list
    val foldn         : (int -> 'a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val subs          : Sloc.Subst.t -> t -> t
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (Index.t -> Field.t -> unit) -> t -> unit
    val indices       : t -> Index.t list
    val bindings      : t -> (Index.t * Field.t) list

    val set_structinfo : t -> structinfo -> t
    val get_structinfo : t -> structinfo

    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module Store:
  sig
    type t = T.store

    val empty        : t
    val bindings     : 'a prestore -> (Sloc.t * 'a preldesc) list * (Sloc.t * 'a precfun) list
    val abstract     : t -> t
    val join_effects :
      t ->
      effectset ->
      (Sloc.t * (T.ldesc * effectptr)) list * (Sloc.t * (T.cfun * effectptr)) list
    val domain       : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val closed       : t -> t -> bool
    val reachable    : t -> Sloc.t -> Sloc.t list
    val restrict     : t -> Sloc.t list -> t
    val map          : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val map_variances : ('a prectype -> 'b prectype) ->
                        ('a prectype -> 'b prectype) ->
                        'a prestore ->
                        'b prestore
    val map_ldesc    : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a prestore -> 'a prestore
    val partition    : (Sloc.t -> bool) -> t -> t * t
    val remove       : t -> Sloc.t -> t
    val upd          : t -> t -> t
      (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
          overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : CType.t -> t -> bool
    val indices      : t -> Index.t list

    val data         : t -> t
    
    val d_store_addrs: unit -> t -> Pretty.doc
    val d_store      : unit -> t -> Pretty.doc

    module Data: sig
      val add           : t -> Sloc.t -> LDesc.t -> t
      val bindings      : t -> (Sloc.t * LDesc.t) list
      val domain        : t -> Sloc.t list
      val mem           : t -> Sloc.t -> bool
      val ensure_sloc   : t -> Sloc.t -> t
      val find          : t -> Sloc.t -> LDesc.t
      val find_or_empty : t -> Sloc.t -> LDesc.t
      val map           : (CType.t -> CType.t) -> t -> t

      val fold_fields   : ('a -> Sloc.t -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
      val fold_locs     : (Sloc.t -> LDesc.t -> 'a -> 'a) -> 'a -> t -> 'a
    end

    module Function: sig
      val add       : 'a prestore -> Sloc.t -> 'a precfun -> 'a prestore
      val bindings  : 'a prestore -> (Sloc.t * 'a precfun) list
      val domain    : t -> Sloc.t list
      val mem       : 'a prestore -> Sloc.t -> bool
      val find      : 'a prestore -> Sloc.t -> 'a precfun
      val fold_locs : (Sloc.t -> 'b precfun -> 'a -> 'a) -> 'a -> 'b prestore -> 'a
    end

    module Unify: sig
      exception UnifyFailure of Sloc.Subst.t * t

      val unify_ctype_locs : t -> Sloc.Subst.t -> CType.t -> CType.t -> t * Sloc.Subst.t
      val unify_overlap    : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> t * Sloc.Subst.t
      val add_field        : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> Field.t -> t * Sloc.Subst.t
      val add_fun          : t -> Sloc.Subst.t -> Sloc.t -> T.cfun -> t * Sloc.Subst.t
    end
  end

  module CFun:
  sig
    type t = T.cfun

    val d_cfun          : unit -> t -> Pretty.doc
    val map             : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val map_variances   : ('a prectype -> 'b prectype) ->
                          ('a prectype -> 'b prectype) ->
                          'a precfun ->
                          'b precfun
    val map_ldesc       : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a precfun -> 'a precfun
    val apply_effects   : (effectptr -> effectptr) -> t -> t
    val well_formed     : Store.t -> t -> bool
    val normalize_names :
      t ->
      t ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> T.ctype -> T.ctype) ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> effectptr -> effectptr) ->
      t * t
    val same_shape      : t -> t -> bool
    val quantified_locs : t -> Sloc.t list
    val instantiate     : CilMisc.srcinfo -> t -> t * Sloc.Subst.t
    val make            : (string * CType.t) list -> Sloc.t list -> Store.t -> CType.t -> Store.t -> effectset -> t
    val subs            : t -> Sloc.Subst.t -> t
    val indices         : t -> Index.t list 
  end

  module Spec:
  sig
    type t = T.spec

    val empty   : t

    val map : ('a prectype -> 'b prectype) -> 'a prespec -> 'b prespec
    val add_fun : bool -> string -> CFun.t * specType -> t -> t
    val add_var : bool -> string -> CType.t * specType -> t -> t
    val add_data_loc : Sloc.t -> LDesc.t * specType -> t -> t
    val add_fun_loc  : Sloc.t -> CFun.t * specType -> t -> t
    
    val store   : t -> Store.t
    val funspec : t -> (T.cfun * specType) Misc.StringMap.t
    val varspec : t -> (T.ctype * specType) Misc.StringMap.t
    val locspectypes : t -> specType Sloc.SlocMap.t

    val make    : (T.cfun * specType) Misc.StringMap.t -> 
                  (T.ctype * specType) Misc.StringMap.t -> 
                  Store.t ->
                  specType Sloc.SlocMap.t ->
                  t

    val add     : t -> t -> t
    val d_spec  : unit -> t -> Pretty.doc
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

module Make (T: CTYPE_DEFS) : S with module T = T

module IndexTypes : CTYPE_DEFS with module R = IndexRefinement
module I          : S with module T = IndexTypes

val d_specTypeRel : unit -> specType -> Pretty.doc
val specTypeMax   : specType -> specType -> specType

(******************************************************************************)
(*************************** Convenient Type Aliases **************************)
(******************************************************************************)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap
    
val null_fun     : Index.t precfun

val void_ctype   : ctype
val ptr_ctype    : ctype
val scalar_ctype : ctype
val fptr_ctype   : ctype

val vtype_to_ctype : Cil.typ -> ctype  

val d_ctype      : unit -> ctype -> Pretty.doc

(**********************************************************************)
(********************** refctypes and friends *************************)
(**********************************************************************)
module ReftTypes : CTYPE_DEFS with module R = Reft
module RefCTypes : S with module T = ReftTypes

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
val refstore_partition  : (Sloc.t -> bool) -> refstore -> refstore * refstore
val refstore_write      : Cil.location -> refstore -> refctype -> refctype -> refstore
val refstore_read       : Cil.location -> refstore -> refctype -> reffield


val refldesc_subs       : refldesc -> (int -> Index.t -> refctype -> refctype) -> refldesc

val is_soft_ptr         : Cil.location -> refstore -> refctype -> bool 
val addr_of_refctype    : Cil.location -> refctype -> Sloc.t * Index.t  
val index_of_ctype      : ctype -> Index.t

val ctype_of_refctype   : refctype -> ctype
val reft_of_refctype    : refctype -> FixConstraint.reft
val cfun_of_refcfun     : refcfun  -> cfun
val store_of_refstore   : refstore -> store
val cspec_of_refspec    : refspec  -> cspec
val args_of_refcfun     : refcfun  -> (string * refctype) list
val ret_of_refcfun      : refcfun  -> refctype 
val stores_of_refcfun   : refcfun  -> refstore * refstore


val d_reft              : unit -> FixConstraint.reft -> Pretty.doc 

