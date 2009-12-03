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

type wld = FixInterface.cilenv * FixInterface.refstore * CilTag.t option

val inwld_of_block      : t -> int -> wld 
val outwld_of_block     : t -> int -> wld 
val add_wld             : int -> wld -> t -> t

val location_of_block   : t -> int -> Cil.location
val annotstmt_of_block  : t -> int -> Refanno.block_annotation * Cil.stmt
val tag_of_instr        : t -> int -> int -> Cil.location -> CilTag.t 
val phis_of_block       : t -> int -> Cil.varinfo list 
val guard_of_block      : t -> int -> int option -> Ast.pred
val csto_of_block       : t -> int -> FixInterface.refstore

val add_cons            : FixConstraint.wf list * FixConstraint.t list * FixConstraint.dep list -> t -> t

val get_cons            : t -> FixConstraint.wf list * FixConstraint.t list * FixConstraint.dep list
val get_fname           : t -> string 
val get_astore          : t -> FixInterface.refstore
val is_undefined        : t -> Cil.varinfo -> bool

val ctype_of_varinfo    : t -> Cil.varinfo -> Ctypes.ctype
val ctype_of_expr       : t -> Cil.exp -> Ctypes.ctype
val refctype_of_global  : t -> Cil.varinfo -> FixInterface.refctype
val create              : CilTag.o -> 
                          FixInterface.cilenv ->
                          FixInterface.refstore ->
                          Ssa_transform.ssaCfgInfo -> 
                          Inferctypes.shape -> t
