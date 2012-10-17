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

module So = Ast.Sort
module Sy = Ast.Symbol

open FixMisc.Ops


type appDef  = { sy_name  : Sy.t
               ; sy_arity : int
               ; sy_sort  : So.t
               ; sy_emb   : Z3.context -> Z3.sort list -> Z3.ast list -> Z3.ast
               }

type sortDef = { so_name  : Ast.Sort.tycon
               ; so_arity : int
               ; so_emb   : Z3.context -> Z3.sort list -> Z3.sort 
               }

(* API *)
let sort_name d = d.so_name
let sym_name d  = d.sy_name
let sym_sort d  = d.sy_sort

(***************************************************************************)
(******************** Theory of Sets ***************************************)
(***************************************************************************)

let set_Set : sortDef = 
  { so_name  = So.tycon "Set_Set" 
  ; so_arity = 1 
  ; so_emb   = fun c -> function 
                 [t] -> Z3.mk_set_sort c t
                 | _ -> assertf "set_Set: type mismatch"
  }  

let set_emp : appDef  = failwith "TBD"
let set_mem : appDef  = failwith "TBD"
let set_sng : appDef  = failwith "TBD"
let set_cup : appDef  = failwith "TBD"
let set_cap : appDef  = failwith "TBD"
let set_dif : appDef  = failwith "TBD"

let set_theory        = ( [ set_Set ]
                        , [ set_emp
                          ; set_sng
                          ; set_cup
                          ; set_cap
                          ; set_dif ] )

(***************************************************************************)
(********* Wrappers Around Z3 Constructors For Last-Minute Checking ********)
(***************************************************************************)

let app_sort_arity def = match So.func_of_t def.sy_sort with
  | Some (n,_,_) -> n
  | None         -> assertf "Theories: app with non-function symbol %s" 
                    (Sy.to_string def.sy_name)

(* API *)
let mk_thy_app def c ts es = 
  asserts (List.length ts = app_sort_arity def) 
    "Theories: app with mismatched sorts %s" (Sy.to_string def.sy_name);
  asserts (List.length es = def.sy_arity) 
    "Theories: app with mismatched args %s" (Sy.to_string def.sy_name);
  def.sy_emb c ts es

(* API *)
let mk_thy_sort def c ts = 
  asserts (List.length ts = def.so_arity) 
    "Theories: app with mismatched sorts %s" (So.tycon_string def.so_name);
  def.so_emb c ts 

(* API *)
let theories () = set_theory

(*
let symbols  () = 
  Misc.map_partial begin function 
    | Sym {sy_name = x; sy_sort = t} -> Some (x, t) 
    | _                              -> None
  end (theories ())

*)
