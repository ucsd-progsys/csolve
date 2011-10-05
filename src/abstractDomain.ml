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

type bind = Ix.t
type t    = Ix.t Asm.t

let empty = Asm.empty

let read sol =
  fun k -> 
    if Asm.mem k sol
    then
      [Sct.pred_of_index_ref (Asm.find k sol) |> snd] (*wrong*)
    else
      [Ast.pFalse]

let read_bind sol k = if Asm.mem k sol then Asm.find k sol else IBot

let top sol xs =
  let xsTop = List.map (fun x -> x, top) xs in
  let xsMap = Asm.of_list xsTop in
    Asm.extend xsMap sol
      
let refine sol c =
  let rhs = F.rhs_of_t c in
  let lhsVal = index_of_reft (F.env_of_t c) sol (F.lhs_of_t c) in
  let refineK sol k =
    let oldK = read_bind sol k in
    let newK = widen oldK lhsVal in
    let _ =  if !Constants.trace_scalar then
      let _ = Format.printf "%a" (F.print_t None) c in
      let _ = Pretty.printf "lhs %a old %a new %a\n" d_index lhsVal d_index oldK d_index newK in
	()
    in
      if (Asm.mem k sol) && oldK = newK
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
  let lhsVal = index_of_reft (F.env_of_t c) sol (F.lhs_of_t c) in
  let onlyK (sub, sym) = (* true if the constraint is unsatisfied *)
	if Asm.mem sym sol then
	  not (is_subindex lhsVal (Asm.find sym sol))
	else
	  true
  in
    List.map onlyK rhsKs |> List.fold_left (&&) true
	
let create cfg =
  let replace v = IBot in
    Asm.map replace cfg.Config.bm
	
let print ppf sol =
  let pf key value =
    Format.fprintf ppf "%s |-> %s\n" (As.to_string key) (repr value) in
  let _ = Asm.mapi pf sol in
    ()

let print_stats ppf sol =
  ()

let dump sol =
  Constants.cprintf Constants.ol_solve 
    "SolnCluster: nothing here yet\n"

let mkbind qbnds = IBot
(* end *)
  
