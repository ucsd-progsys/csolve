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
val create: FixConstraint.wf list * FixConstraint.t list * FixConstraint.dep list -> t
val add: t -> string -> Ssa_transform.ssaCfgInfo -> FixConstraint.wf list * FixConstraint.t list * FixConstraint.dep list -> t
val print: FixConstraint.soln option -> unit -> t -> Pretty.doc

(* val get_wfs: t -> FixConstraint.wf list 
   val get_cs: t -> FixConstraint.t list
   val get_deps: t -> FixConstraint.dep list *)


val solve: t -> Ast.Qualifier.t list -> string -> FixConstraint.soln * FixConstraint.t list 

val force: t -> string -> Ast.Qualifier.t list -> (FixConstraint.envt * FixConstraint.reft) Ast.Symbol.SMap.t -> Ast.pred Ast.Symbol.SMap.t 

(*
 
val force: t -> Ast.Qualifier.t list -> string -> Ast.pred YM.t 
val force: t -> FixConstraint.soln -> Ast.pred list -> FixConstraint.envt -> FixConstraint.reft -> Ast.pred list
*)
