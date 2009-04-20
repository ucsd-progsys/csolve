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
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(* This module implements basic datatypes and operations on constraints *)

module S  = Ast.Symbol
module E  = Ast.Expression
module P  = Ast.Predicate
module SM = S.SMap

type tag  = int
type subs = (S.t * E.t) list                    (* [x,e] *)
type refa = Conc of P.t | Kvar of subs * S.t
type reft = S.t * (refa list)                   (* VV, [ra] *)
type envt = (Sort.t * reft) SM.t
type soln = P.t list SM.t
type t    = envt * P.t * reft * reft * (tag option) 

val is_simple        : t -> bool
val sol_read         : soln -> S.t -> P.t list
val group_sol_update : soln -> (S.t * P.t) list -> (bool * soln)
val print            : soln option -> Format.formatter -> t -> unit
val to_string        : t -> string 
