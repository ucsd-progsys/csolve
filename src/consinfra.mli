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

type t
type wld = FixInterface.cilenv * FixInterface.refstore
val annotstmt_of_block: t -> int -> (Refanno.block_annotation * Cil.stmt)
val location_of_block: t -> int -> Cil.location
val phis_of_block: t -> int -> Cil.varinfo list 
val inwld_of_block: t -> int -> wld 
val outwld_of_block: t -> int -> wld 
val guard_of_block: t -> int -> Ast.pred

val add_wld     : int -> wld -> t -> t
val add_cons    : Constraint.wf list -> Constraint.t list -> t -> t
val get_cons    : t -> Constraint.wf list * Constraint.t list
val get_fname   : t -> FixInterface.name
val get_astore  : t -> FixInterface.refstore
val is_undefined: t -> Cil.varinfo -> bool

val ctype_of_varinfo: t -> Cil.varinfo -> Ctypes.ctype
val ctype_of_expr: t -> Cil.exp -> Ctypes.ctype
val create: FixInterface.cilenv 
         -> Ssa_transform.ssaCfgInfo 
         -> (Inferctypes.ctemap * Ctypes.store) 
         -> (Refanno.block_annotation array * Refanno.ctab)
         -> t


(* Deprecated *)         
(* val stmt_of_block: t -> int -> Cil.stmt *)
