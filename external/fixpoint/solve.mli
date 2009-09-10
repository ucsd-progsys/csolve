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

(** This module implements a fixpoint solver *)

type t 

val create  :  Ast.Sort.t list                  (* New sorts, now = []*)  
            -> Ast.Sort.t Ast.Symbol.SMap.t     (* New operators, now = [] *)
            -> Ast.pred list                    (* New axioms, now = [] *)
            -> int                              (* Tag arity *)
            -> FixConstraint.dep list           (* Dependencies *)
            -> FixConstraint.t list             (* Subtyping Constraints *)
            -> FixConstraint.wf list            (* WF Constraints *)
            -> Ast.Qualifier.t list             (* Qualifiers *)
            -> (t * FixConstraint.soln)         (* Solver Instance, 
                                                   Post-WF solution *) 

val solve   :  t                                (* Solver Instance *)
            -> FixConstraint.soln               (* Starting Solution *)
            -> (FixConstraint.soln * (FixConstraint.t list))    (* Fixpoint Solution, 
                                                                   Unsat Constraints *)

val save    : string -> t -> FixConstraint.soln -> unit 
