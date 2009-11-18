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

type name
type cilenv

type refctype = (Ctypes.index * FixConstraint.reft) Ctypes.prectype
type refcfun  = (Ctypes.index * FixConstraint.reft) Ctypes.precfun
type refldesc (*= (Ctypes.index * FixConstraint.reft) Ctypes.precfun *)
type refstore = (Ctypes.index * FixConstraint.reft) Ctypes.prestore
type refspec  = (Ctypes.index * FixConstraint.reft) Ctypes.PreSpec.t

val d_refstore          : unit -> refstore -> Pretty.doc
val d_refctype          : unit -> refctype -> Pretty.doc
val d_refcfun           : unit -> refcfun -> Pretty.doc

val ctype_of_refctype   : refctype -> Ctypes.index Ctypes.prectype
val cfun_of_refcfun     : refcfun  -> Ctypes.index Ctypes.precfun
val refcfun_of_cfun     : Ctypes.cfun -> refcfun
val store_of_refstore   : refstore -> Ctypes.store
val cspec_of_refspec    : refspec -> Ctypes.cspec

val qlocs_of_refcfun    : refcfun  -> Sloc.t list
val args_of_refcfun     : refcfun  -> (string * refctype) list
val ret_of_refcfun      : refcfun  -> refctype 
val stores_of_refcfun   : refcfun  -> refstore * refstore
val mk_refcfun          : Sloc.t list -> (string * refctype) list -> refstore -> refctype -> refstore -> refcfun 

val name_of_string      : string -> name
val name_of_varinfo     : Cil.varinfo -> name
val name_fresh          : unit -> name

val skolem              : unit -> Ast.expr

val ce_rem              : name -> cilenv -> cilenv 
val ce_mem              : name -> cilenv -> bool 
val ce_empty            : cilenv
val ce_adds             : cilenv -> (name * refctype) list -> cilenv
val ce_find             : name -> cilenv -> refctype
val ce_mem_fn           : string -> cilenv -> bool
val ce_adds_fn          : cilenv -> (string * refcfun) list -> cilenv
val ce_find_fn          : string -> cilenv -> refcfun
val d_cilenv            : unit -> cilenv -> Pretty.doc

val t_fresh_fn          : Ctypes.cfun  -> refcfun
val t_fresh             : Ctypes.ctype -> refctype
val t_true              : Ctypes.ctype -> refctype
val t_true_refctype     : refctype -> refctype
val t_zero_refctype     : refctype -> refctype
val t_pred              : Ctypes.ctype -> Ast.Symbol.t -> Ast.pred -> refctype
val t_exp               : cilenv -> Ctypes.ctype -> Cil.exp -> refctype
val t_name              : cilenv -> name -> refctype
val t_ctype_refctype    : Ctypes.ctype -> refctype -> refctype

val t_subs_names        : (name * name) list -> refctype -> refctype
val t_subs_exps         : (name * Cil.exp) list -> refctype -> refctype
val t_subs_locs         : Sloc.Subst.t -> refctype -> refctype

val refstore_empty      : refstore
val binds_of_refldesc   : Sloc.t -> refldesc -> (name * refctype) list
val refstore_mem        : Sloc.t -> refstore -> bool
val refstore_remove     : Sloc.t -> refstore -> refstore
val refstore_set        : refstore -> Sloc.t -> refldesc -> refstore
val refstore_get        : refstore -> Sloc.t -> refldesc
val refldesc_subs       : refldesc -> (int -> Ctypes.ploc -> refctype -> refctype) -> refldesc

val refstore_write      : Cil.location -> refstore -> refctype -> refctype -> refstore
val refstore_read       : Cil.location -> refstore -> refctype -> refctype
val refstore_fresh      : string -> Ctypes.store -> refstore

val refstore_subs       : Cil.location -> ('a -> refctype -> refctype) -> 'a -> refstore -> refstore
val refstore_subs_locs  : Cil.location -> (Sloc.t * Sloc.t) list -> refstore -> refstore

val is_soft_ptr         : Cil.location -> refstore -> refctype -> bool 
val sorts               : Ast.Sort.t list

val make_wfs            : cilenv -> refctype -> CilTag.t -> FixConstraint.wf list
val make_wfs_fn         : cilenv -> refcfun -> CilTag.t -> FixConstraint.wf list
val make_wfs_refstore   : cilenv -> refstore -> CilTag.t -> FixConstraint.wf list

val make_cs             : cilenv -> Ast.pred -> 
                          refctype -> refctype -> 
                          CilTag.t option -> CilTag.t -> Cil.location -> 
                          FixConstraint.t list * FixConstraint.dep list
val make_cs_validptr    : cilenv -> Ast.pred ->
                          refctype -> CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list
val make_cs_refldesc    : cilenv -> Ast.pred -> 
                          (Sloc.t * refldesc) -> (Sloc.t * refldesc) -> 
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list
val make_cs_refstore    : cilenv -> Ast.pred -> 
                          refstore -> refstore -> bool ->
                          CilTag.t option -> CilTag.t -> Cil.location ->
                          FixConstraint.t list * FixConstraint.dep list
val make_dep            : bool -> CilTag.t option -> CilTag.t option -> FixConstraint.dep 
val annot_dump          : string -> FixConstraint.soln -> unit
