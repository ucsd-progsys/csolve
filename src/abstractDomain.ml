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

module Ix = Index
module A  = Ast
module As = A.Symbol
module Ac = A.Constant  
module Asm = As.SMap  
module Sct = ScalarCtypes
module F   = FixConstraint
module FAI = FixAstInterface

open Ix  
open Misc.Ops

(* Instead of mapping k variables below to single indices, instead
   keep around some previously mapped indices *)
type kstore = {
  l  : int;      (* length of ks *)
  ks : Ix.t list (* history *)
}

let empty_ks = { l = 1; ks = [IBot] }
let soln_of_kstore_map = Asm.map (fun kst -> List.hd kst.ks)

(* look at a list of indices. count:
   (# consecutive decreases in ub,
    # consecutive increases in ub,
    # consecutive decreases in lb,
    # consecutive increases in lb) *)
let count_bounds is =
  let change b1 r b2 c = match b1, b2 with
    | Some b1, Some b2 -> if r b1 b2 then c+1 else 0
    | _ -> 0
  in
  let count ((ubd,ubi,lbd,lbi),i') i = match i', i with
    | ICClass { ub = ub'; lb = lb'  },
      ICClass { ub = ub; lb = lb } ->
	(change ub' (<) ub ubd, change ub' (>) ub ubi,
	 change lb' (<) lb lbd, change lb' (>) lb lbd), i
    | _ -> (0,0,0,0),i
  in
    List.fold_left count ((0,0,0,0),List.hd is) (List.tl is) |> fst

let rec take n lst = 
  if n <= 0 then
    []
  else
     (List.hd lst)::(take (n-1) (List.tl lst))

(* How to add a value to a kstore *)
let refine_store n {l = l; ks = ks} i =
  if l = 0 then
    { l = 1; ks = [i] }
  else if l < n then
    let i' = lub i (List.hd ks) in
    { l = l+1; ks = i'::ks }
  else
    let i' = lub i (List.hd ks) in
      match i' with
	| ICClass { ub = ub; lb = lb; m = m; c = c; } ->
	    let (ubd,ubi,lbd,lbi) = count_bounds (i'::ks) in
	    let ub' = Misc.choose (ubi >= n) None ub in
	    let lb' = Misc.choose (lbd >= n) None lb in
	    let i' = ICClass {ub = ub'; lb = lb'; m = m; c = c} in
	       {l = l; ks = i'::(take (l-1) ks)}
	| _ -> {l = l; ks = i'::(take (l-1) ks)}

type bind = Ix.t
type t    = kstore Asm.t

let empty = Asm.empty

let read sol =
  fun k -> 
    if Asm.mem k sol
    then
      [Sct.pred_of_index_ref (List.hd (Asm.find k sol).ks) |> snd] (*wrong*)
    else
      [Ast.pFalse]

let read_bind sol k =
  if Asm.mem k sol then
    List.hd (Asm.find k sol).ks
  else
    IBot

let read_store sol k =
  if Asm.mem k sol then
    (Asm.find k sol)
  else
    empty_ks

let top sol xs =
  let xsTop = List.map (fun x -> x, {l = 1; ks = [top]}) xs in
  let xsMap = Asm.of_list xsTop in
    Asm.extend xsMap sol
      
let refine sol c =
  let ixmap = soln_of_kstore_map sol in
  let rhs = F.rhs_of_t c in
  let lhsVal = index_of_reft (F.env_of_t c) ixmap (F.lhs_of_t c) in
  let refineK sol k =
    let oldK = read_store sol k in
    let newK = refine_store 3 oldK lhsVal (* widen oldK lhsVal in *)
(*    let _ =  if !Constants.trace_scalar then
      let _ = Format.printf "%a" (F.print_t None) c in
      let _ = Pretty.printf "lhs %a old %a new %a\n" d_index lhsVal d_index oldK d_index newK in
	() *)
    in
      if (Asm.mem k sol) && List.hd oldK.ks = List.hd newK.ks
      then (false, sol)
      else (true, Asm.add k newK sol)
  in
    List.fold_left
      begin fun (chg, sol) (_, sym) -> 
	let (chg', sol') = refineK sol sym in (chg || chg', sol')
      end
      (false, sol) (F.kvars_of_reft rhs)

let unsat sol c =
  (* Make sure that lhs <= k for each k in rhs *)
  let rhsKs = F.rhs_of_t c |> F.kvars_of_reft  in
  let ixmap = soln_of_kstore_map sol in
  let lhsVal = index_of_reft (F.env_of_t c) ixmap (F.lhs_of_t c) in
  let onlyK (sub, sym) = (* true if the constraint is unsatisfied *)
    not (is_subindex lhsVal (read_bind sol sym))
  in
    List.map onlyK rhsKs |> List.fold_left (&&) true
	
let create cfg =
  let replace v = empty_ks in
    Asm.map replace cfg.Config.bm
	
let print ppf sol =
  let pf key value =
    List.fold_left (^) "" (List.map repr value.ks)
    |> Format.fprintf ppf "%s |-> %s\n" (As.to_string key)
 in
  let _ = Asm.mapi pf sol in
    ()

let print_stats ppf sol =
  ()

let dump sol =
  Constants.cprintf Constants.ol_solve 
    "SolnCluster: nothing here yet\n"

let mkbind qbnds = IBot
(* end *)
  
