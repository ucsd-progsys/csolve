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
 *
 *)


(** This module implements a fixpoint solver *)

module P  = Ast.Predicate
module E  = Ast.Expression
module PH = Ast.Predicate.Hash
module Sy = Ast.Symbol
module SM = Sy.SMap
module So = Ast.Sort
module C  = Constraint
module Co = Constants

type t = {
  val tpc : TP.t;
  val sri : Ci.t;
}

(*************************************************************)
(********************* Stats *********************************)
(*************************************************************)

let stat_refines        = ref 0
let stat_simple_refines = ref 0
let stat_tp_refines     = ref 0
let stat_imp_queries    = ref 0
let stat_valid_queries  = ref 0
let stat_matches        = ref 0

(* Sections :
 * Iterative Refinement
 * Constraint Sat Checking
 * Refinement
 * Constraint Indexing

 * Debug/Profile
 * Stats/Printing
 * Misc Helpers
  
 * TypeDefs 
 * Initial Solution
 * Qual Instantiation
 * Constraint Simplification/Splitting *)

(***************************************************************)
(************************** Refinement *************************)
(***************************************************************)

let is_simple_refatom = function 
  | C.Kvar ([], _) -> true
  | _ -> false

let is_simple_constraint (_,_,(_,ra1s),(_,ra2s),_) = 
  List.for_all is_simple_refatom ra1s &&
  List.for_all is_simple_refatom ra2s &&
  not (!Cf.no_simple || !Cf.verify_simple)

let lhs_preds s env gp r1 =
  let envps = environment_preds s env in
  let r1ps  = refinement_preds  s r1 in
  gp :: (envps ++ r1ps) 

let rhs_cands s = function
  | C.Kvar (xes, k) -> 
      sol_read s k |> 
      List.map (fun q -> ((k,q), apply_substs xes q))
  | _ -> []

let check_tp me env lps =  function [] -> [] | rcs ->
  let env = (SM.map fst env)
  let rv  = Misc.do_catch "ERROR: check_tp" 
              (TP.set_and_filter me.tpc (SM.map fst env) lps) rcs in
  let _   = stat_tp_refines += 1;
            stat_imp_queries += (List.length rcs);
            stat_valid_queries += (List.length rv) in
  rv

let refine me s ((env, g, (vv1, ra1s), (vv2, ra2s), _) as c) =
  let _  = asserts (vv1 = vv2) "ERROR: malformed constraint";
           incr stat_refines in
  let lps  = lhs_preds s env g (vv1, ra1s) in
  let rcs  = Misc.flap (rhs_cands s) ra2s in
  if (List.exists P.is_contra lps) || (rcs = []) then
    let _ = stat_matches += (List.length rcs) in
    (false, s)
  else
    let rcs     = List.filter (fun (_,p) -> not (P.is_contra p)) rcs in
    let lt      = PH.create 17 in
    let _       = List.iter (fun p -> PH.add lt p ()) lps in
    let (x1,x2) = List.partition (fun (_,p) -> PH.mem lt p) rcs in
    let _       = stat_matches += (List.length x1) in
    let kqs1    = List.map fst xs in
    (if is_simple_constraint c
     then let _ = stat_simple_refines += 1 in kqs1 
     else kqs1 ++ (check_tp me env lps x2))
    |> group_and_update s 

(***************************************************************)
(************************* Satisfaction ************************)
(***************************************************************)

let unsat me s ((env, gp, (vv1, ra1s), (vv2, ra2s), _) as c) =
  let _   = asserts (vv1 = vv2) "ERROR: malformed constraint" in
  let lps = lhs_preds s env gp (vv1, ra1s) in
  let rhs = [(0, P.And (Misc.flap (refineatom_preds s) ra2s))] in
  not ((check_tp me env lps rhs) = [0])

let unsat_constraints me s sri =
  Ci.to_list sri |> List.filter (unsat me s)

(***************************************************************)
(******************** Iterative Refinement *********************)
(***************************************************************)

let rec acsolve me w s = 
  let _ = if !stat_refines mod 100 = 0 
          then Co.cprintf Co.ol_solve "num refines =%d \n" !stat_refines in
  let _ = if Co.ck_olev Co.ol_insane then dump_solution s in
  match Ci.pop me.sri si.wkl with (None,_) -> s | (Some c, w') ->
    let ch  = BS.time "refine" (refine me s) c in
    let w'' = if ch then Ci.get_ref_deps me.sri c |> Ci.push me.sri w' else w' in 
    acsolve me w'' s 

(* API *)
let solve me s = 
  let _ = Ci.to_list me.sri |> dump_constraints; 
          dump_solution s; 
          dump_solving me s 0 in
  let w = BS.time "init wkl"    Ci.winit me.sri in 
          BS.time "solving sub" (acsolve me s) w;
          TP.reset ();
          dump_solution s; 
          dump_solving sri s 1 in
  let u = BS.time "testing solution" (unsat_constraints sri) s in
  let _ = if u != [] then Format.printf "Unsatisfied Constraints:\n %a"
                          (Misc.pprint_many true "\n" (C.print None)) u in
  (s, u)

(* API *)
let create ts ps cs =
  let tpc = TP.create () in
  let _   = BS.time "Adding sorts"     TP.new_sorts tpc ts in
  let _   = BS.time "Adding axioms"   (TP.new_axioms tpc) ps in
  let sri = BS.time "Making ref index" Ci.create cs in
  { tpc = tpc; sri = src }


(***********************************************************************)
(************** FUTURE WORK:  A Parallel Solver ************************)
(***********************************************************************)

(*
let Par.reduce f = fun (x::xs) -> Par.fold_left f x xs

let one_solve sis s = 
  Par.map (fun si -> Solve.solve si s) sis |> 
  Par.reduce (fun (s,b) (s',b') -> (Constraint.join s s', b || b'))

(* API *)
let psolve n ts axs cs s0 = 
  let css = partition cs n in
  let sis = pmap (Solve.create ts axs) css in
  Misc.fixpoint (one_solve sis) s0
*)
