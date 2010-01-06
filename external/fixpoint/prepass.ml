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

let mydebug = false 

(***************************************************************)
(*********** Input Constraint & Solution Validation ************)
(***************************************************************)

exception Out_of_scope of Ast.Symbol.t 



(* 1. check ids are distinct, return max id *)
let phase1 cs =
  let ids = Misc.map_partial C.ido_of_t cs in
  let _   = asserts (Misc.distinct ids) "Invalid Constraints 1" in
  (List.fold_left max 0 ids), cs

(* 2. add distinct ids to each constraint *)
let phase2 (tmax, cs) =
  Misc.mapfold begin fun j c -> match C.ido_of_t c with
    | None -> (j+1, C.make_t (C.env_of_t c) (C.grd_of_t c) (C.lhs_of_t c) (C.rhs_of_t c) (Some j) (C.tag_of_t c))
    | _    -> (j, c)
  end (tmax + 1) cs
  |> snd

(* 3. check that sorts are consistent across constraints *)
let phase3 cs =
  let memo = Hashtbl.create 17 in
  List.iter begin fun c ->
    let env = C.env_of_t c in
    let id  = C.id_of_t c in
    let (vv1,t1,_) = C.lhs_of_t c in
    let (vv2,t2,_) = C.rhs_of_t c in
    let _ = if not (vv1 = vv2 && t1 = t2) then 
      let _ = Format.printf "Invalid Constraints 3a in \n %a " (C.print_t None) c in
      let _ = 0/0 in () in
    SM.iter begin fun x (_,t,_) ->
      try asserts (t = (Hashtbl.find memo x)) "Invalid Constraints 3b: %d" id
      with Not_found -> Hashtbl.replace memo x t
    end env
  end cs;
  cs

(* 4. check that each tag has the same arity [a] *)
let phase4 a cs =
  List.iter begin fun c -> 
    asserts (a = List.length (C.tag_of_t c)) "Invalid Constraints 4"
  end cs;
  cs

(* 5. check that all refinements are well-formed *)
let validate_vars env msg vs = 
  List.iter begin fun v -> 
    if not(SM.mem v env) then 
      let _ = F.printf "ERROR: out_of_scope variable %a (%s)" Sy.print v (Lazy.force msg) in
      raise (Out_of_scope v)
  end vs 

let validate_reft s env msg ((vv,t,_) as r) =
  let env = SM.add vv r env in
  r |> BS.time "preds_of_reft" (C.preds_of_reft s)
    |> Misc.flap (BS.time "support" P.support)
    |> BS.time "validate_vars" (validate_vars env msg)

let validate_binding s env msg x r =
  let msg = lazy (Format.sprintf "%s binding for %s " (Lazy.force msg) (Sy.to_string x)) in
  validate_reft s env msg r

let phase5 s cs =
  Misc.filter begin fun c ->
    let msg  = C.to_string c in
    let env  = C.env_of_t c in
    let lhs  = C.lhs_of_t c in
    let rhs  = C.rhs_of_t c in
    BS.time "valid binds" (SM.iter (validate_binding s env (lazy (msg^"\n BAD ENV")))) env;
    BS.time "valid lhs" (validate_reft s env (lazy (msg^"\n BAD LHS"))) lhs;
    BS.time "valid rhs" (validate_reft s env (lazy (msg^"\n BAD RHS"))) rhs;
    true
  end cs

let validate a s cs =
  let f a s = phase1 <+> phase2 <+> phase3 <+> phase4 a <+> phase5 s in 
  BS.time "validate shapes" (f a s) cs

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
  let (tsz, tcsz, tksz, tesz) = 
    IM.fold begin fun i (sz, csz, ksz, esz) (tsz, tcsz, tksz, tesz) -> 
      Co.cprintf Co.ol_solve
        "ctag %d: binds=%d, cbinds=%d, kbinds=%d, ebinds=%d \n" 
         i sz csz ksz esz;
      (tsz + sz, tcsz + csz, tksz + ksz, tesz + esz)
    end im (0,0,0,0) in
  Co.cprintf Co.ol_solve_stats 
    "Total binds=%d, cbinds=%d, kbinds=%d, ebinds=%d \n" 
    tsz tcsz tksz tesz

let profile1 sri = 
  sri |> Cindex.to_list
      |> List.fold_left profile_cstr IM.empty
      |> dump_profile

let key_of_cstr c = 
  c |> C.env_of_t 
    |> C.bindings_of_env 
    |> List.map fst 
    |> List.map Sy.to_string 
    |> List.sort compare 
    |> String.concat "," 

let profile2 sri =
  sri |> Cindex.to_list
      |> Misc.groupby key_of_cstr 
      |> List.length
      |> fun n -> Co.cprintf Co.ol_solve_stats "Constraint Clusters = %d \n" n

let profile sri = 
  profile1 sri;
  profile2 sri

