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
 *)


module Misc = FixMisc

type symDef  = { sy_name  : Ast.Symbol.t
               ; sy_arity : int
               ; sy_sort  : Ast.Sort.t
               ; sy_emb   : Z3.context -> Z3.sort list -> Z3.ast list -> Z3.ast
               }

type sortDef = { so_name  : Ast.Sort.tycon
               ; so_arity : int
               ; so_emb   : Z3.context -> Z3.sort list -> Z3.sort 
               }

type def     = Sym of symDef | Sort of sortDef

type t       = def list


(***************************************************************************)
(******************** Theory of Sets ***************************************)
(***************************************************************************)

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

let theories () = 
  failwith "TBD: implement theories!"

let symbols  () = 
  Misc.map_partial begin function 
    | Sym {sy_name = x; sy_sort = t} -> Some (x, t) 
    | _                              -> None
  end (theories ())
