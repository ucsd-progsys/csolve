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

(* This module implements the IMP language *)

(* We can have at most one set of temporaries in scope at a time
 * so we share named and mark temporaries *)

type var   = PVar of Ast.Symbol.t
           | TVar of Ast.Symbol.t

type kvar  = Ast.Subst.t * Ast.Symbol.t

type decl  = RDecl of Ast.Symbol.t * Ast.Symbol.t list
           | PDecl of Ast.Symbol.t

(* IMP commands *)

type tupl  = var list

type instr = Assm of Ast.pred list
           | Asst of Ast.pred list
           | Asgn of var * var
           | Rget of Ast.Symbol.t * tupl
           | Rset of tupl * Ast.Symbol.t
           | Havc of var

type block = instr list

type program = decl list * block list

val print_program : Format.formatter -> program -> unit
val print_program_as_c : Format.formatter -> program -> unit

val check_imp : program -> bool
