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

(* This file is part of the liquidC Project.*)
(*
module AlocMap : sig
  type t
  val id   : t
  val add  : Sloc.t -> Sloc.t -> t -> t
  val mem  : Sloc.t -> t -> bool
  val find : Sloc.t -> t -> Sloc.t
end
*)

type name = Ast.Symbol.t
type cilenv

type alocmap  = Sloc.t -> Sloc.t option

(* moved to ctypes.ml/i
module Reft      : Ctypes.CTYPE_REFINEMENT with type t = Ctypes.Index.t * FixConstraint.reft

module RefCTypes : Ctypes.S with module R = Reft
type refctype = RefCTypes.CType.t
type refcfun  = RefCTypes.CFun.t
type refldesc = RefCTypes.LDesc.t
type refstore = RefCTypes.Store.t
type refspec  = RefCTypes.Spec.t
type reffield = RefCTypes.Field.t

val d_refstore          : unit -> refstore -> Pretty.doc
val d_refctype          : unit -> refctype -> Pretty.doc
val d_refcfun           : unit -> refcfun -> Pretty.doc

*)

val ctype_of_refctype   : Ctypes.refctype -> Ctypes.ctype
val cfun_of_refcfun     : Ctypes.refcfun  -> Ctypes.cfun
val refcfun_of_cfun     : Ctypes.cfun -> Ctypes.refcfun
val store_of_refstore   : Ctypes.refstore -> Ctypes.store
val cspec_of_refspec    : Ctypes.refspec -> Ctypes.cspec

val qlocs_of_refcfun    : Ctypes.refcfun  -> Sloc.t list
val args_of_refcfun     : Ctypes.refcfun  -> (string * Ctypes.refctype) list
val ret_of_refcfun      : Ctypes.refcfun  -> Ctypes.refctype 
val stores_of_refcfun   : Ctypes.refcfun  -> Ctypes.refstore * Ctypes.refstore
val mk_refcfun          : Sloc.t list -> (string * Ctypes.refctype) list -> Ctypes.refstore -> Ctypes.refctype -> Ctypes.refstore -> Ctypes.refcfun 

val pred_of_refctype    : FixConstraint.soln -> Cil.varinfo -> Ctypes.refctype -> Ast.pred
val name_of_string      : string -> name
val name_of_varinfo     : Cil.varinfo -> name
val name_fresh          : unit -> name

val ce_rem              : name -> cilenv -> cilenv 
val ce_mem              : name -> cilenv -> bool 
val ce_empty            : cilenv
val ce_adds             : cilenv -> (name * Ctypes.refctype) list -> cilenv
val ce_find             : name -> cilenv -> Ctypes.refctype
val ce_mem_fn           : string -> cilenv -> bool
val ce_adds_fn          : cilenv -> (string * Ctypes.refcfun) list -> cilenv
val ce_find_fn          : string -> cilenv -> Ctypes.refcfun
val d_cilenv            : unit -> cilenv -> Pretty.doc

(* EW:
val extend_world        : refldesc -> 
                          (name * refctype) list -> 
                          Sloc.t -> bool -> 
                          (cilenv * refstore * 'a) -> 
                          (cilenv * refstore * 'a)
*)
val extend_world        : alocmap -> Ctypes.refstore -> Sloc.t -> Sloc.t -> bool ->
                          (Ctypes.refldesc -> Ctypes.refldesc) ->
                          Cil.location -> CilTag.t ->
                          (cilenv * Ctypes.refstore * 'a) -> 
                          (cilenv * Ctypes.refstore * 'a) * FixConstraint.t list

val strengthen_final_field :
  Ctypes.IndexSet.t ->
  string ->
  Ctypes.Index.t ->
  Ctypes.reffield ->
  Ctypes.reffield

(*
val t_fresh_fn          : (* (Sloc.t -> Sloc.t) -> *) Ctypes.cfun  -> refcfun
*)

(*
val eApp_skolem         : Ast.expr -> Ast.expr 
val get_skolems         : unit -> Ast.expr list
val t_skolem            : Ctypes.ctype -> refctype
*)

val map_fn              : (Ctypes.refctype -> Ctypes.refctype) -> Ctypes.refcfun -> Ctypes.refcfun


val eApp_bbegin         : Ast.expr -> Ast.expr 

val t_scalar_zero       : Ctypes.refctype
val t_scalar            : Ctypes.ctype -> Ctypes.refctype
val t_fresh             : Ctypes.ctype -> Ctypes.refctype
val t_true              : Ctypes.ctype -> Ctypes.refctype
val t_true_refctype     : Ctypes.refctype -> Ctypes.refctype
val t_zero_refctype     : Ctypes.refctype -> Ctypes.refctype
val t_scalar_refctype   : Ctypes.refctype -> Ctypes.refctype
val t_pred              : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> Ctypes.refctype
val t_size_ptr          : Ctypes.ctype -> int -> Ctypes.refctype
val t_exp               : cilenv -> Ctypes.ctype -> Cil.exp -> Ctypes.refctype
val t_exp_scalar        : Cil.varinfo -> Cil.exp -> Ctypes.refctype
val t_name              : cilenv -> name -> Ctypes.refctype
val t_ctype_refctype    : Ctypes.ctype -> Ctypes.refctype -> Ctypes.refctype

val t_subs_names        : (name * name) list -> Ctypes.refctype -> Ctypes.refctype
val t_subs_exps         : (name * Cil.exp) list -> Ctypes.refctype -> Ctypes.refctype
val t_subs_locs         : Sloc.Subst.t -> Ctypes.refctype -> Ctypes.refctype

val may_contain_deref   : Ctypes.refctype -> bool

val new_block_reftype   : (* (Sloc.t -> Sloc.t) -> *) Ctypes.refctype -> Ctypes.refctype


val is_poly_cloc        : Ctypes.refstore -> Sloc.t -> bool

(* MOVE TO CTYPES 
val refstore_empty      : Ctypes.refstore
val refstore_mem        : Sloc.t -> Ctypes.refstore -> bool
val refstore_remove     : Sloc.t -> Ctypes.refstore -> Ctypes.refstore
val refstore_set        : Ctypes.refstore -> Sloc.t -> Ctypes.refldesc -> Ctypes.refstore
val refstore_get        : Ctypes.refstore -> Sloc.t -> Ctypes.refldesc
val refstore_fold       : (Sloc.t -> Ctypes.refldesc -> 'a -> 'a) -> Ctypes.refstore -> 'a -> 'a
val refstore_partition  : (Sloc.t -> bool) -> Ctypes.refstore -> Ctypes.refstore * Ctypes.refstore

val refldesc_subs       : Ctypes.refldesc -> (int -> Ctypes.Index.t -> Ctypes.refctype -> Ctypes.refctype) -> Ctypes.refldesc

*)

val refstore_strengthen_addr :
  Cil.location ->
  cilenv ->
  Ctypes.refstore ->
  Ctypes.IndexSet.t Sloc.SlocMap.t ->
  string ->
  Ctypes.refctype ->
  cilenv * Ctypes.refstore

val refstore_fresh             : (* (Sloc.t -> Sloc.t) -> *) string -> Ctypes.store -> Ctypes.refstore

val refstore_subs       : (* Cil.location -> *) ('a -> Ctypes.refctype -> Ctypes.refctype) -> 'a -> Ctypes.refstore -> Ctypes.refstore
val refstore_subs_locs  : (* Cil.location -> *) (Sloc.t * Sloc.t) list -> Ctypes.refstore -> Ctypes.refstore


val sorts               : Ast.Sort.t list
val axioms              : Ast.pred list
val builtinm            : FixConstraint.reft Ast.Symbol.SMap.t

val make_wfs            : alocmap -> cilenv -> Ctypes.refstore -> Ctypes.refctype -> CilTag.t -> FixConstraint.wf list
val make_wfs_fn         : alocmap -> cilenv -> Ctypes.refcfun -> CilTag.t -> FixConstraint.wf list
val make_wfs_refstore   : alocmap -> cilenv -> Ctypes.refstore -> Ctypes.refstore -> CilTag.t -> FixConstraint.wf list

val make_cs             : alocmap -> cilenv -> Ast.pred -> 
                          Ctypes.refctype -> Ctypes.refctype -> 
                          CilTag.t option -> CilTag.t -> Cil.location -> 
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_validptr    : alocmap -> cilenv -> Ast.pred ->
                          Ctypes.refctype -> CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refldesc    : alocmap -> cilenv -> Ast.pred -> 
                          (Sloc.t * Ctypes.refldesc) -> (Sloc.t * Ctypes.refldesc) -> 
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refstore    : alocmap -> cilenv -> Ast.pred -> 
                          Ctypes.refstore -> Ctypes.refstore -> bool ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list
val make_dep            : bool -> CilTag.t option -> CilTag.t option -> FixConstraint.dep 
val annot_dump          : FixConstraint.soln -> unit
val annot_clear         : 'a -> unit
(* val annot_binds         : unit -> (FixConstraint.envt * FixConstraint.reft) Ast.Symbol.SMap.t
*)

val quals_of_file       : string -> Ast.Qualifier.t list 

