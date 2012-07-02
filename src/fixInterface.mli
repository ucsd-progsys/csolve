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

type result = { soln   : FixConstraint.soln
              ; unsats : FixConstraint.t list
              ; ucones : (FixConstraint.tag Ast.Cone.t) list
              }

type cilenv

val pred_of_refctype    : FixConstraint.soln -> Cil.varinfo -> Ctypes.refctype -> Ast.pred
val sort_of_prectype    : 'a Ctypes.prectype -> Ast.Sort.t
val ce_rem              : FixAstInterface.name -> cilenv -> cilenv 
val ce_mem              : FixAstInterface.name -> cilenv -> bool 
val ce_empty            : cilenv
val ce_adds             : cilenv -> (FixAstInterface.name * Ctypes.refctype) list -> cilenv
val ce_find             : FixAstInterface.name -> cilenv -> Ctypes.refctype
val ce_mem_fn           : string -> cilenv -> bool
val ce_adds_fn          : cilenv -> (string * Ctypes.refcfun) list -> cilenv

val ce_find_fn          : string -> cilenv -> Ctypes.refcfun

val d_cilenv            : unit -> cilenv -> Pretty.doc
val extend_world        : Ctypes.refstore -> Sloc.t -> Sloc.t -> bool ->
                          (Ctypes.refldesc -> Ctypes.refldesc) ->
                          Cil.location -> CilTag.t ->
                          (cilenv * Ctypes.refstore * 'a) -> 
                          (cilenv * Ctypes.refstore * 'a) * FixConstraint.t list

val strengthen_type_with_deref :
  Ast.expr ->
  int ->
  Ctypes.refctype ->
  Ctypes.refctype

val strengthen_final_field :
  Index.IndexSet.t ->
  string ->
  Index.t ->
  Ctypes.reffield ->
  Ctypes.reffield


val map_fn              : (Ctypes.refctype -> Ctypes.refctype) -> Ctypes.refcfun -> Ctypes.refcfun

val e_false             : Sloc.t -> Ctypes.effectptr
val e_true              : Sloc.t -> Ctypes.effectptr
val e_fresh             : Sloc.t -> Ctypes.effectptr

val vv_addr             : Ast.Symbol.t
val vv_addr_expr        : Ast.expr
val replace_addr        : Cil.varinfo -> Ctypes.refctype -> Ctypes.refctype

val t_scalar            : Ctypes.ctype -> Ctypes.refctype
val t_fresh             : Ctypes.ctype -> Ctypes.refctype
val t_true              : Ctypes.ctype -> Ctypes.refctype
val t_zero              : Ctypes.ctype -> Ctypes.refctype
val t_field_at_block_of : string -> int -> Ctypes.ctype -> Ctypes.refctype
val t_true_refctype     : Ctypes.refctype -> Ctypes.refctype
val t_false_refctype    : Ctypes.refctype -> Ctypes.refctype
val t_zero_refctype     : Ctypes.refctype -> Ctypes.refctype
val t_scalar_refctype   : Ctypes.refctype -> Ctypes.refctype
val t_indexpred_refctype : Ctypes.refctype -> Ctypes.refctype
val t_nullterm_refctype : Ctypes.refctype -> Ctypes.refctype
val t_pred              : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> Ctypes.refctype
val t_spec_pred         : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> Ctypes.refctype
val t_size_ptr          : Ctypes.ctype -> int -> Ctypes.refctype
val t_valid_ptr         : Ctypes.ctype -> Ctypes.refctype
val t_start_ptr         : Ctypes.ctype -> Ctypes.refctype
val t_ptr_footprint     : cilenv -> Cil.varinfo -> Ctypes.refctype
val t_fptr_footprint    : cilenv -> Cil.varinfo -> Ctypes.refctype
val t_exp               : cilenv -> Ctypes.ctype -> Cil.exp -> Ast.pred option * Ctypes.refctype
val t_exp_scalar        : Cil.varinfo -> Cil.exp -> Ctypes.refctype
val t_name              : cilenv -> FixAstInterface.name -> Ctypes.refctype
val t_ctype_refctype    : Ctypes.ctype -> Ctypes.refctype -> Ctypes.refctype
val t_addr              : Sloc.t -> Ctypes.refctype
val t_fresh_fn          : Ctypes.refcfun -> Ctypes.refcfun
val t_subs_names        : (FixAstInterface.name * FixAstInterface.name) list -> Ctypes.refctype -> Ctypes.refctype
val t_subs_exps         : (FixAstInterface.name * Cil.exp) list -> Ctypes.refctype -> Ctypes.refctype
val t_subs_locs         : Sloc.Subst.t -> Ctypes.refctype -> Ctypes.refctype
val t_singleton_effect  : Ctypes.refctype -> EffectDecls.t -> Ctypes.refctype

val name_of_sloc_index  : Sloc.t -> Index.t -> FixAstInterface.name

val subs_of_lsubs : 
  Sloc.Subst.t ->
  Ctypes.refstore ->
  (FixAstInterface.name * FixAstInterface.name) list
    
val rename_refctype :
  Sloc.Subst.t ->
  (Ast.Symbol.t * Cil.exp) list ->
  Ctypes.refctype -> 
  Ctypes.refctype

val may_contain_deref   : Ctypes.refctype -> bool

val new_block_reftype   : (* (Sloc.t -> Sloc.t) -> *) Ctypes.refctype -> Ctypes.refctype


val is_poly_cloc        : Ctypes.refstore -> Sloc.t -> bool

val refcfun_of_cfun     : Ctypes.cfun -> Ctypes.refcfun

val refcspec_of_cspec   : Ctypes.cspec -> Ctypes.refspec


val refstore_strengthen_addr :
  Cil.location ->
  cilenv ->
  Ctypes.refstore ->
  Index.IndexSet.t Sloc.SlocMap.t ->
  string ->
  Ctypes.refctype ->
  cilenv * Ctypes.refstore

val refstore_fresh             : string -> Ctypes.store -> Ctypes.refstore
val conv_refstore_bottom       : Ctypes.refstore -> Ctypes.refstore
val conv_effectset_bottom      : Ctypes.effectset -> Ctypes.effectset

val refstore_subs       : ('a -> Ctypes.refctype -> Ctypes.refctype) -> 'a -> Ctypes.refstore -> Ctypes.refstore
val refstore_subs_locs  : (Sloc.t * Sloc.t) list -> Ctypes.refstore -> Ctypes.refstore

val effectset_subs      : ('a -> Ctypes.effectptr -> Ctypes.effectptr) -> 'a -> Ctypes.effectset -> Ctypes.effectset
val effectset_subs_locs : (Sloc.t * Sloc.t) list -> Ctypes.refstore -> Ctypes.effectset -> Ctypes.effectset

val make_wfs            : cilenv -> Ctypes.refstore -> Ctypes.refctype -> FixConstraint.wf list
val make_wfs_fn         : cilenv -> Ctypes.refcfun -> FixConstraint.wf list
val make_wfs_refstore   : cilenv -> Ctypes.refstore -> Ctypes.refstore -> FixConstraint.wf list
val make_wfs_effectset  : cilenv -> Ctypes.refstore -> Ctypes.effectset -> FixConstraint.wf list

val make_cs             : cilenv -> Ast.pred -> 
                          Ctypes.refctype -> Ctypes.refctype -> 
                          CilTag.t option -> CilTag.t -> Cil.location -> 
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_effect_weaken_type :
                          cilenv -> Ast.pred ->
                          Ctypes.refstore -> Ctypes.refctype -> Ctypes.effectptr ->
                          CilTag.t option -> CilTag.t -> Cil.location -> 
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_effect_weaken_var :
                          cilenv -> Ast.pred ->
                          Ctypes.refstore -> Cil.varinfo -> EffectDecls.t -> Ctypes.effectptr ->
                          CilTag.t option -> CilTag.t -> Cil.location -> 
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_effectset   : cilenv -> Ast.pred ->
                          Ctypes.refstore -> Ctypes.refstore ->
                          Ctypes.effectset -> Ctypes.effectset ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_effectset_binds :
                          bool -> cilenv -> Ast.pred ->
                          (Sloc.t * (Ctypes.refldesc * Ctypes.effectptr)) list ->
                          (Sloc.t * (Ctypes.refldesc * Ctypes.effectptr)) list ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_assert      : cilenv -> Ast.pred ->
                          Ast.pred ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_assert_effectsets_disjoint :
                          cilenv -> Ast.pred -> Ctypes.refstore ->
                          Ctypes.effectset -> Ctypes.effectset ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refldesc    : cilenv -> Ast.pred ->
                          (Sloc.t * Ctypes.refldesc) -> (Sloc.t * Ctypes.refldesc) -> 
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refstore    : cilenv -> Ast.pred -> 
                          Ctypes.refstore -> Ctypes.refstore -> bool ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refstore_binds : cilenv -> Ast.pred ->
                             (Sloc.t * Ctypes.refldesc) list ->
                             (Sloc.t * Ctypes.refldesc) list ->
                             bool ->
                             CilTag.t option -> CilTag.t -> Cil.location ->
                             FixConstraint.t list * FixConstraint.dep list

val make_cs_tuple       : cilenv -> Ast.pred ->
                          Sloc.Subst.t -> (Ast.Symbol.t * Cil.exp) list ->
                          Ctypes.refctype list ->
                          Ctypes.refctype list ->
                          CilTag.t option ->
                          CilTag.t ->
                          Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_cs_refcfun     : cilenv -> Ast.pred ->
                          Ctypes.refcfun -> 
                          Ctypes.refcfun ->
                          CilTag.t ->
                          Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list

val make_dep            : bool -> CilTag.t option -> CilTag.t option -> FixConstraint.dep 
(*val annot_dump          : FixSolution.t -> unit
  val annot_clear         : 'a -> unit
*)

