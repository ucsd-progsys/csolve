(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
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
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONAst.Symbol.
 *
 *)

(* This module implements basic datatypes and operations on constraints *)

type tag  = int
type subs = (Ast.Symbol.t * Ast.expr) list            (* [x := e] *) 
type refa = Conc of Ast.pred | Kvar of subs * Ast.Symbol.t
type reft = Ast.Symbol.t * Ast.Sort.t * (refa list)   (* { VV: t | [ra] } *)
type envt = reft Ast.Symbol.SMap.t
type t    = envt * Ast.pred * reft * reft * (tag option) 
  (*env, guard, lhs, rhs, cid*)

type soln = Ast.pred list Ast.Symbol.SMap.t

type deft = Srt of Ast.Sort.t 
          | Axm of Ast.pred 
          | Cst of t 
          | Sol of Ast.Symbol.t * Ast.pred list
          | Qul of Ast.pred

val get_id           : t -> tag
val get_kvars        : t -> (subs * Ast.Symbol.t) list
val apply_substs     : subs -> Ast.pred -> Ast.pred
val refineatom_preds : soln -> refa -> Ast.pred list
val refinement_preds : soln -> reft -> Ast.pred list
val environment_preds: soln -> envt -> Ast.pred list

val env_of_list      : (Ast.Symbol.t * reft) list -> envt
val is_simple        : t -> bool
val sol_read         : soln -> Ast.Symbol.t -> Ast.pred list
val group_sol_update : soln -> (Ast.Symbol.t * Ast.pred) list -> (bool * soln)
val print            : soln option -> Format.formatter -> t -> unit
val print_soln       : Format.formatter -> soln -> unit
val to_string        : t -> string 

val make_reft        : Ast.Symbol.t -> Ast.Sort.t -> refa list -> reft
val make_t           : envt -> Ast.pred -> reft -> reft -> t
val vv_of_reft       : reft -> Ast.Symbol.t
val so_of_reft       : reft -> Ast.Sort.t
val ras_of_reft      : reft -> refa list
val env_of_t         : t -> envt
val grd_of_t         : t -> Ast.pred
val lhs_of_t         : t -> reft
val rhs_of_t         : t -> reft


