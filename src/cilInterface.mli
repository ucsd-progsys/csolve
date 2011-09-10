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

val index_of_attrs      : Cil.attributes -> Index.t
val ctype_of_cilbasetype: Cil.typ -> Index.t Ctypes.prectype
val expr_of_cilcon      : Cil.constant -> Ast.expr
val stride_of_cilexp    : Cil.exp -> int 
val expr_of_cilexp      : Cil.exp -> Ast.expr
val pred_of_cilexp      : Cil.exp -> Ast.pred
val reft_of_cilexp      : Ast.Symbol.t -> Cil.exp -> (Ast.pred option) * Ast.pred 

val foldGlobalsIf       : (Cil.global -> bool) -> Cil.file -> ('a -> Cil.global -> 'a) -> 'a -> 'a
val iterGlobalsIf       : (Cil.global -> bool) -> Cil.file -> (Cil.global -> unit) -> unit

