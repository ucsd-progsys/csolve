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

(* This module implements the IMP language and translation from fixpoint constraints *)


module F  = Format
module H  = Hashtbl
module A  = Ast
module E  = A.Expression
module P  = A.Predicate
module Sy = A.Symbol
module SM = Sy.SMap
module C  = FixConstraint
module Cg = FixConfig
(*module BS = BNstats*)

open Imp
open Misc.Ops

(* Translation from fixpoint to IMP *)

(* Declarations *)

let filter_wfs cs =
  Misc.maybe_list (List.map (function Cg.Wfc x -> Some x | _ -> None) cs)

let filter_subt cs =
  Misc.maybe_list (List.map (function Cg.Cst x -> Some x | _ -> None) cs)

let wf_to_decls wf =
  let vars  =
    List.map fst (C.bindings_of_env (C.env_of_wf wf)) |>
    Misc.sort_and_compact in
  let kvars = C.kvars_of_reft (C.reft_of_wf wf) in
  (List.map (fun k -> RDecl (snd k, vars)) kvars, List.map (fun v -> PDecl v) vars)

let constraints_to_decls cs =
  let decls = List.map wf_to_decls (filter_wfs cs) in
  let (rdecls, pdecls) = (Misc.flap fst decls, Misc.flap snd decls) in
  rdecls @ pdecls 

(* Constraint translation *)

let rec get_kdecl kvar decls =
  match decls with  
  | RDecl (k, vars) :: decls ->
      if k = kvar then
        vars
      else
        get_kdecl kvar decls
  | _ :: decls -> get_kdecl kvar decls
  | [] -> raise Not_found

let sub_to_assume (var, expr) =
  Assm [A.pAtom (A.eVar var, A.Eq, expr)]

(* [[{t | p}]]_get *)

let get_instrs vv decls (subs, kvar) =
  let vars = get_kdecl kvar decls |> List.map (fun v -> TVar v) in
  let assumes = subs |> Ast.Subst.to_list |> List.map sub_to_assume in
  Rget (kvar, vars) :: assumes @
  [Asgn (PVar vv, List.hd vars)]

let set_instr decls (subs, kvar) =
  Rset (List.map (fun v -> TVar v) (get_kdecl kvar decls), kvar)

let emptySol = PredAbs.read PredAbs.empty

let reft_to_get_instrs decls reft =
  let vv = C.vv_of_reft reft in
  let kvars = C.kvars_of_reft reft in
  let preds = C.preds_of_reft emptySol reft in
  match (kvars, preds) with
  | ([], preds) -> Havc (PVar vv) :: Assm preds :: []
  | (kvars, []) -> Misc.flap (get_instrs vv decls) kvars
  | (kvars, preds) -> Misc.flap (get_instrs vv decls) kvars @ ([Assm preds])

(* [[{t | p}]]_set *)

let reft_to_set_instrs decls reft =
  let kvars = C.kvars_of_reft reft in
  let preds = C.preds_of_reft emptySol reft in
  match (kvars, preds) with
  | ([], preds) -> Asst preds :: []
  | (kvars, []) -> List.map (set_instr decls) kvars
  | (kvars, preds) -> List.map (set_instr decls) kvars @ [(Asst preds)]

(* [[x:T; G]] *)

let binding_to_instrs decls (var, reft) =
  reft_to_get_instrs decls reft @ [Asgn (PVar var, PVar (C.vv_of_reft reft))]

let envt_to_instrs decls envt =
  Misc.flap (binding_to_instrs decls) (C.bindings_of_env envt)

let constraint_to_block decls c =
  let (env, grd, lhs, rhs) =
    (C.env_of_t c, C.grd_of_t c, C.lhs_of_t c, C.rhs_of_t c) in
  Assm [grd] ::
  envt_to_instrs decls env @
  reft_to_get_instrs decls lhs @
  reft_to_set_instrs decls rhs

let constraints_to_blocks decls cs =
  List.map (constraint_to_block decls) (filter_subt cs)

let mk_program cs =
  let decls = constraints_to_decls cs in
  (decls, constraints_to_blocks decls cs)
