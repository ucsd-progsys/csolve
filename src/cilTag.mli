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

type o
type t 
type cause = Raw   of string 
           | Call  of Cil.exp 
           | Deref of Cil.exp 
           | Spec  of string * Cil.varinfo

val create        : Ssa_transform.t list -> o

val make_t        : o -> Cil.location -> string -> int -> int -> cause -> t
val make_global_t : o -> Cil.location -> cause -> t

(*
val loc_of_t      : o -> t -> Cil.location
val fname_of_t    : o -> t -> string
val block_of_t    : o -> t -> int
val t_of_tag      : FixConstraint.tag -> t  (* breaks representation hiding! *)
*)

val loc_of_tag    : o -> FixConstraint.tag -> Cil.location
val cause_of_tag  : o -> FixConstraint.tag -> cause
val d_cause       : o -> unit -> cause -> Pretty.doc

val tag_of_t      : t -> FixConstraint.tag
val d_exp_reSugar : o -> unit -> Cil.exp -> Pretty.doc
val d_instr_reSugar : o -> unit -> Cil.instr -> Pretty.doc

