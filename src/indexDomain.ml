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
module P  = A.Predicate  
module Asm = As.SMap  
module Sct = ScalarCtypes
module F   = FixConstraint
module FAI = FixAstInterface

open Ix  
module Misc = FixMisc open Misc.Ops

(* Instead of mapping k variables below to single indices, instead
   keep around some previously mapped indices *)
type kstore = {
  l  : int;        (* length of ks *)
  ks : Ix.t list   (* history *)
}

let ix_of_kstore k = List.hd k.ks    

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
    { l = 1; ks = [i]}
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
	    let i'  = mk_sequence c m lb' ub' in
	       {l = l; ks = i'::(take (l-1) ks)}
	| _ -> {l = l; ks = i'::(take (l-1) ks)}

type sort_store = (Ast.Sort.t option) * kstore
type bind = Ix.t
type t    = sort_store Asm.t (*might not know the sort*)

let empty = Asm.empty

let read sol =
  fun k ->
    if Asm.mem k sol then
      let t,kstore = Asm.find k sol in
	match t with
	  | None ->
	      [kstore.ks |> List.hd |> Sct.pred_of_index_int |> snd]
	  | Some t' ->
	      if A.Sort.is_int t' then
		[kstore.ks |> List.hd |> Sct.pred_of_index_int |> snd]
	      else
		let p = kstore.ks |> List.hd |> Sct.pred_of_index_ref |> snd in
		let psub = P.subst p (As.value_variable A.Sort.t_int)
		  (A.eVar (As.value_variable t')) in
		  [psub]
    else
      [Ast.pFalse]

let min_read = read

let read_bind sol k =
  if Asm.mem k sol then
    Asm.find k sol |> snd |> fun st -> List.hd st.ks
  else
    IBot

let read_store sol t k =
  if Asm.mem k sol then
    Asm.find k sol
  else
    Some t, {l = 1; ks = [IBot]}

let top sol xs =
  let xsTop = List.map (fun x -> x, (None, {l = 1; ks = [top]})) xs in
  let xsMap = Asm.of_list xsTop in
    Asm.extend xsMap sol
      
let refine sol c =
  let rhs = F.rhs_of_t c in
  let t = F.sort_of_reft (F.lhs_of_t c) in
  let env = F.env_of_t c |> Misc.flip apply_grd (F.grd_of_t c) in
  let lhsVal = index_of_reft env (read_bind sol) (F.lhs_of_t c) in
  let refineK sol k =
    let (kt,oldK) = read_store sol t k in
    let _ = match kt with
      | Some kt' -> assert (Ast.Sort.is_int t = Ast.Sort.is_int kt')
      | _ -> ()
    in
    let newK = refine_store 5 oldK lhsVal in
    let _ =  if !Constants.trace_scalar then
      let _ = Format.printf "%a" (F.print_t None) c in
      let _ = Pretty.printf "lhs %a old %a new %a\n"
	d_index lhsVal
	d_index (ix_of_kstore oldK)
	d_index (ix_of_kstore newK) in
      let _ = Format.print_flush () in
	()
    in
      if (Asm.mem k sol) && List.hd oldK.ks = List.hd newK.ks
      then (false, sol)
      else (true, Asm.add k (kt,newK) sol)
  in
    List.fold_left
      begin fun (chg, sol) (_, sym) -> 
	let (chg', sol') = refineK sol sym in (chg || chg', sol')
      end
      (false, sol) (F.kvars_of_reft rhs)

let unsat sol c =
  (* Make sure that lhs <= k for each k in rhs *)
  let rhsKs = F.rhs_of_t c |> F.kvars_of_reft  in
  let env = F.env_of_t c |> Misc.flip apply_grd (F.grd_of_t c)in
  let lhsVal = index_of_reft env (read_bind sol) (F.lhs_of_t c) in
  (* let rhsVal = index_of_reft env (read_bind sol) (F.rhs_of_t c) in *)
       (* not (is_subindex lhsVal rhsVal) *)
  let onlyK (sub, sym) = (* true if the constraint is unsatisfied *)
    not (is_subindex lhsVal (read_bind sol sym))
  in
    List.map onlyK rhsKs |> List.fold_left (||) false
	
let create cfg _ =
  let kss = Misc.flap
    begin fun wf -> 
      let reft = F.reft_of_wf wf in
      let sort = F.sort_of_reft reft in
	List.map (fun k -> (sort, k)) (F.kvars_of_reft reft)
    end cfg.FixConfig.ws
  in
    List.fold_left
      begin fun m (sort,(sub,k)) ->
	let _ = assert (not (Asm.mem k m)) in
	  Asm.add k (Some sort, {l = 1; ks = [IBot]}) m
      end Asm.empty kss
    
let print ppf sol =
  let pf key (t,i) =
   Format.fprintf ppf "%s -> %s\n" (As.to_string (key)) (repr (read_bind sol key)) in
  let _ = Asm.mapi pf sol in
    ()

let print_stats ppf sol =
  ()

let dump sol = ()

    
let mkbind qbnds = IBot

let ctr_examples _ _ _ = failwith "No counterexamples for IndexDomain"

(* end *)
  
