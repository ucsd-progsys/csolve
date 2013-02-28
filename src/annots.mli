(*
 * Copyright © 1990-2011 The Regents of the University of California. All rights reserved. 
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

type rhs    = AsgnE of Cil.instr | AsgnV of Cil.varinfo 

(*type binder = N of FixAstInterface.name | S of string | I of Index.t | Nil
 *)

type vbind  = Ctypes.binder * (Ctypes.refctype * Cil.typ)

type binding = TVar of vbind 
             | TFun of string * (Ctypes.refcfun  * Cil.fundec)
             | TSto of string * Ctypes.refstore 
             | TSSA of string * Ssa_transform.vmap_t
             | TAsg of Cil.varinfo * ((Cil.location * rhs) list)

(* val d_binder : unit -> Ctypes.binder -> Pretty.doc *)

val deconstruct_fun : (string * Ctypes.refcfun * Cil.fundec) -> vbind list


val annot_shape   : Shape.t FixMisc.StringMap.t 
                  -> Ssa_transform.t FixMisc.StringMap.t 
                  -> Ctypes.refcfun FixMisc.StringMap.t
                  -> unit
val annot_var     : FixAstInterface.name -> Ctypes.refctype -> unit
val annot_sto     : string -> Ctypes.refstore -> unit
val annot_asgn    : Cil.varinfo -> Cil.location -> rhs -> unit 
val clear         : unit -> unit
val dump_annots   : FixConstraint.soln option -> unit
val dump_infspec  : Ctypes.refstore 
                    -> CilMisc.dec list
                    -> Qualifier.t list
                    -> FixConstraint.soln -> unit
val dump_bindings : unit -> binding list
