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


(** This module implements a fixpoint solver *)
module BS = Bstats
module F  = Format
module A  = Ast
module Co = Constants
module P  = A.Predicate
module E  = A.Expression
module S  = A.Sort
module PH = A.Predicate.Hash
module Sy = A.Symbol
module SM = Sy.SMap
module C  = Constraint
module Ci = Cindex
module TP = TheoremProverZ3.Prover

open Misc.Ops

type t = {
  tpc : TP.t;
  sri : Ci.t;
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


(***************************************************************)
(************************** Refinement *************************)
(***************************************************************)

let lhs_preds s env gp r1 =
  let envps = C.environment_preds s env in
  let r1ps  = C.refinement_preds  s r1 in
  gp :: (envps ++ r1ps) 

let rhs_cands s = function
  | C.Kvar (xes, k) -> 
      C.sol_read s k |> 
      List.map (fun q -> ((k,q), C.apply_substs xes q))
  | _ -> []

let check_tp me env vv t lps =  function [] -> [] | rcs ->
  let env = SM.map snd3 env |> SM.add vv t in
  let rv  = Misc.do_catch "ERROR: check_tp" 
              (TP.set_filter me.tpc env vv lps) rcs in
  let _   = stat_tp_refines    += 1;
            stat_imp_queries   += (List.length rcs);
            stat_valid_queries += (List.length rv) in
  rv

let refine me s ((env, g, ((vv1, t1, _) as r1), (_, _, ra2s), _) as c) =
  let _   = stat_refines += 1 in
  let lps = lhs_preds s env g r1 in
  let rcs = Misc.flap (rhs_cands s) ra2s in
  if (List.exists P.is_contra lps) || (rcs = []) then
    let _ = stat_matches += (List.length rcs) in
    (false, s)
  else
    let rcs     = List.filter (fun (_,p) -> not (P.is_contra p)) rcs in
    let lt      = PH.create 17 in
    let _       = List.iter (fun p -> PH.add lt p ()) lps in
    let (x1,x2) = List.partition (fun (_,p) -> PH.mem lt p) rcs in
    let _       = stat_matches += (List.length x1) in
    let kqs1    = List.map fst x1 in
    (if C.is_simple c 
     then (stat_simple_refines += 1; kqs1) 
     else kqs1 ++ (check_tp me env vv1 t1 lps x2))
    |> C.group_sol_update s 

(***************************************************************)
(************************* Satisfaction ************************)
(***************************************************************)

let unsat me s (env, gp, ((vv,t,_) as r1), r2, _) =
  let lps    = lhs_preds s env gp r1 in
  let rhsp   = r2 |> thd3 |> Misc.flap (C.refineatom_preds s) |> A.pAnd in
  not ((check_tp me env vv t lps [(0, rhsp)]) = [0])

let unsat_constraints me s =
  Ci.to_list me.sri |> List.filter (unsat me s)

(***************************************************************)
(************************ Debugging/Stats **********************)
(***************************************************************)

let print_solution_stats ppf s = 
  let (sum, max, min) =   
      (SM.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
       SM.fold (fun _ qs x -> max x (List.length qs)) s min_int,
       SM.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
  let avg = (float_of_int sum) /. (float_of_int (Sy.sm_length s)) in
  F.fprintf ppf "# variables   = %d \n" (Sy.sm_length s);
  F.fprintf ppf "# Quals: Total=%d, Avg=%f, Max=%d, Min=%d \n" sum avg max min

let print_solver_stats ppf me = 
  let cs   = Ci.to_list me.sri in 
  let cn   = List.length cs in
  let scn  = List.length (List.filter C.is_simple cs) in
  (* F.fprintf ppf "%a" Ci.print me.sri; *)
  F.fprintf ppf "# constraints = %d \n" cn;
  F.fprintf ppf "# simple constraints = %d \n" scn;
  F.fprintf ppf "# Refine Iterations = %d (si=%d tp=%d unsatLHS=%d) \n"
    !stat_refines !stat_simple_refines !stat_tp_refines !stat_matches;
  F.fprintf ppf "#Queries = %d@ (TP=%d, valid=%d)\n" 
    !stat_matches !stat_imp_queries !stat_valid_queries;
  F.fprintf ppf "%a" TP.print_stats me.tpc

let dump me s = 
  F.printf "%a \n" print_solver_stats me;
  F.printf "%a \n" print_solution_stats s

(***************************************************************)
(********************** Input Validation ***********************)
(***************************************************************)

(* 1. check tags are distinct, return max tag *)
let phase1 cs = 
  let tags = Misc.map_partial (fun (_,_,_,_,x) -> x) cs in
  let _    = asserts (Misc.distinct tags) "Invalid Constraints 3" in
  List.fold_left max 0 tags 

(* 2. add distinct tags to each constraint *) 
let phase2 cs tmax = 
  List.fold_left 
    (fun (cs, j) c -> match c with
       | (_,_,_,_, Some i) -> (c::cs, j)
       | (a,b,c,d, None)   -> ((a,b,c,d, Some j)::cs, j+1))
    ([], tmax+1) cs
  |> fst

(* 3. check that sorts are consistent across constraints *)
let phase3 cs =
  let memo = Hashtbl.create 17 in
  let _    = List.iter begin fun (env,_,(vv1,t1,_),(vv2,t2,_),_) ->
               asserts (vv1 = vv2 && t1 = t2) "Invalid Constraints 1"; 
               SM.iter begin fun x (_,t,_) ->
                 try asserts (t = (Hashtbl.find memo x)) 
                       "Invalid Constraints 2" 
                 with Not_found -> 
                   Hashtbl.replace memo x t
               end env
             end cs in
  cs

let validate cs = 
  phase1 cs |> phase2 cs |> phase3

(***************************************************************)
(******************** Iterative Refinement *********************)
(***************************************************************)

let rec acsolve me w s = 
  let _ = if !stat_refines mod 100 = 0 
          then Co.cprintf Co.ol_solve "num refines =%d \n" !stat_refines in
  let _ = if Co.ck_olev Co.ol_insane then F.printf "%a" C.print_soln s in
  let _ = Co.cprintf Co.ol_solve "At iter %d " !stat_refines in
  match Ci.wpop me.sri w with (None,_) -> s | (Some c, w') ->
    let (ch, s')  = BS.time "refine" (refine me s) c in
    let w''       = if ch then Ci.deps me.sri c |> Ci.wpush me.sri w' else w' in 
    acsolve me w'' s' 

(* API *)
let solve me (s : C.soln) = 
  let _ = Co.cprintf Co.ol_solve "%a" Ci.print me.sri;  
          Co.cprintf Co.ol_solve "%a" C.print_soln s;
          dump me s in
  let w = BS.time "init wkl"    Ci.winit me.sri in 
  let s = BS.time "solving sub" (acsolve me w) s in
  let _ = dump me s in
  let u = BS.time "testing solution" (unsat_constraints me) s in
  let _ = if u != [] then F.printf "Unsatisfied Constraints:\n %a"
                          (Misc.pprint_many true "\n" (C.print None)) u in
  (s, u)

(* API *)
let create ts sm ps cs =
  let tpc = TP.create ts sm ps in
  let cs  = validate cs in
  let sri = BS.time "Making ref index" Ci.create cs in
  { tpc = tpc; sri = sri }



let save fname me s =
  let oc  = open_out fname in
  let ppf = F.formatter_of_out_channel oc in
  Ci.iter  
    (F.fprintf ppf "constraint: @[%a@] \n" (C.print None))
    me.sri;
  SM.iter 
    (fun k ps -> 
      F.fprintf ppf "solution: @[%a := [%a]@] \n"  
        Sy.print k (Misc.pprint_many false ";" P.print) ps)
    s




(*
(***********************************************************************)
(************** FUTURE WORK:  A Parallel Solver ************************)
(***********************************************************************)

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
