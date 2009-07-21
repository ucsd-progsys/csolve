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


(** This module implements various constraint validation and simplification 
 *  prepasses *)

module BS = BNstats
module F  = Format
module A  = Ast
module Co = Constants
module P  = A.Predicate
module E  = A.Expression
module S  = A.Sort
module Q  = A.Qualifier
module PH = A.Predicate.Hash
module Sy = A.Symbol
module SM = Sy.SMap
module IM = Misc.IntMap 
module C  = FixConstraint
open Misc.Ops

let mydebug = true

(***************************************************************)
(******************** Constraint Validation ********************)
(***************************************************************)

exception Out_of_scope of Ast.Symbol.t 

let validate_vars env msg vs = 
  List.iter begin fun v -> 
    if not(SM.mem v env) then 
      let _ = F.printf "ERROR: out_of_scope variable %a (%s)" Sy.print v msg in
      raise (Out_of_scope v)
  end vs 

let validate_reft s env msg r =
  r |> C.preds_of_reft s 
    |> Misc.flap P.support 
    |> validate_vars env msg

let validate_binding s env msg x r =
  let msg = Format.sprintf "%s binding for %s " msg (Sy.to_string x) in
  validate_reft s env msg r

let validate s sri =
  Cindex.to_list sri 
  |> List.for_all begin fun c ->
       let msg  = Format.sprintf "tag %d" (C.id_of_t c) in
       let (vv,t,_) as r = C.lhs_of_t c in
       let env  = C.env_of_t c |> SM.add vv r in
       let lhs  = C.lhs_of_t c in
       let rhs  = C.rhs_of_t c in
       try 
         SM.iter (validate_binding s env msg) env;
         validate_reft s env (" LHS "^msg) lhs;
         validate_reft s env (" RHS "^msg) rhs;
         true
       with Out_of_scope _ -> false
     end 

(*
let force_phase1c s cs c =
  match phase1c s c with
  | Some (_, bvs) ->
      let env =
        List.fold_left (fun e v -> SM.add v dfty e) (C.env_of_t c) bvs in
      (C.make_t env (C.grd_of_t c) (C.lhs_of_t c) (C.rhs_of_t c) (Some (C.id_of_t c))) :: cs
  | None -> c :: cs 

let force_phase1 soln cs = List.fold_left (force_phase1c soln) [] cs
let validate soln cs = if not (phase1 soln cs) then assert false
let force_validate soln cs = force_phase1 soln cs
*)

(***************************************************************)
(****************** Pruning Unconstrained Vars *****************)
(***************************************************************)

let rhs_ks cs =
  cs  |> Misc.flap (Misc.compose C.kvars_of_reft C.rhs_of_t)
      |> List.fold_left (fun rhss (_, kv) -> Sy.SSet.add kv rhss) Sy.SSet.empty

let unconstrained_kvars cs =
  let rhss = rhs_ks cs in
  cs  |> Misc.flap C.kvars_of_t
      |> List.map snd
      |> List.filter (fun kv -> not (Sy.SSet.mem kv rhss))

let true_unconstrained s sri =
  sri |> Cindex.to_list 
      |> unconstrained_kvars 
      |> List.fold_left (fun s kv -> SM.add kv [] s) s

(***************************************************************)
(*********************** Constraint Profiling  *****************)
(***************************************************************)

let profile_cstr im c = 
  SM.fold begin fun _ (_,_,rs) ((a, b, c, d) as pfl) -> 
    match rs with [] -> (a, b, c, d+1)  | _::_ -> begin 
      List.fold_left begin fun (sz, csz, ksz, esz) r -> match r with 
        | C.Conc _  -> (sz+1, csz+1, ksz, esz) 
        | _         -> (sz+1, csz, ksz+1, esz)
      end pfl rs
    end
  end (C.env_of_t c) (0,0,0,0)
  |> fun pfl -> IM.add (C.id_of_t c) pfl im


let dump_profile im =
  if mydebug then 
    let (tsz, tcsz, tksz, tesz) = 
      IM.fold begin fun i (sz, csz, ksz, esz) (tsz, tcsz, tksz, tesz) -> 
        Format.printf "ctag %d: binds=%d, cbinds=%d, kbinds=%d, ebinds=%d \n" i sz csz ksz esz;
        (tsz + sz, tcsz + csz, tksz + ksz, tesz + esz)
      end im (0,0,0,0) in
    Format.printf "Total binds=%d, cbinds=%d, kbinds=%d, ebinds=%d \n" tsz tcsz tksz tesz

let profile sri = 
  sri |> Cindex.to_list
      |> List.fold_left profile_cstr IM.empty
      |> dump_profile
