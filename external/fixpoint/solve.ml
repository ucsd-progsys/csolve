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
module C  = FixConstraint
module Ci = Cindex
module TP = TpZ3.Prover
module PP = Prepass

open Misc.Ops

type t = {
  tpc : TP.t;
  sri : Ci.t;
  ws  : C.wf list;
}

let mydebug = false  

(*************************************************************)
(********************* Stats *********************************)
(*************************************************************)

let stat_refines        = ref 0
let stat_simple_refines = ref 0
let stat_tp_refines     = ref 0
let stat_imp_queries    = ref 0
let stat_valid_queries  = ref 0
let stat_matches        = ref 0
let stat_unsatLHS       = ref 0
let stat_cfreqt         = Hashtbl.create 37

let hashtbl_incr_frequency t k = 
  let n = try Hashtbl.find t k with Not_found -> 0 in
  Hashtbl.replace t k (n+1)

let hashtbl_print_frequency t = 
  Misc.hashtbl_to_list t 
  |> Misc.groupby snd
  |> List.map (function ((_,n)::_) as xs -> (n, List.length xs)) 
  |> List.sort compare
  |> List.iter (fun (n,m) -> Format.printf "ITERFREQ: %d times %d constraints \n" n m)


(***************************************************************)
(************************** Refinement *************************)
(***************************************************************)

let rhs_cands s = function
  | C.Kvar (xes, k) -> 
      C.sol_read s k |> 
      List.map (fun q -> ((k,q), P.substs q xes))
  | _ -> []

let check_tp me env vv t lps =  function [] -> [] | rcs ->
  let env = SM.map snd3 env |> SM.add vv t in
  let rv  = Misc.do_catch "ERROR: check_tp" 
              (TP.set_filter me.tpc env vv lps) rcs in
  let _   = ignore(stat_tp_refines    += 1);
            ignore(stat_imp_queries   += (List.length rcs));
            ignore(stat_valid_queries += (List.length rv)) in
  rv

let refine me s c =
  let _   = stat_refines += 1 in
  let env = C.env_of_t c in
  let (vv1, t1, _) = C.lhs_of_t c in
  let (_,_,ra2s) as r2 = C.rhs_of_t c in
  let k2s = C.kvars_of_reft r2 |> List.map snd in
  let lps = BS.time "preds_of_lhs" (C.preds_of_lhs s) c in
  let rcs = BS.time "rhs_cands" (Misc.flap (rhs_cands s)) ra2s in
  if (BS.time "lhs_contra" (List.exists P.is_contra) lps) || (rcs = []) then
    let _ = stat_unsatLHS += 1 in
    let _ = stat_matches  += (List.length rcs) in
    (false, s)
  else
    let rcs     = List.filter (fun (_,p) -> not (P.is_contra p)) rcs in
    let lt      = PH.create 17 in
    let _       = List.iter (fun p -> PH.add lt p ()) lps in
    let (x1,x2) = List.partition (fun (_,p) -> PH.mem lt p) rcs in
    let _       = stat_matches += (List.length x1) in
    let kqs1    = List.map fst x1 in
    (if C.is_simple c 
     then (ignore(stat_simple_refines += 1); kqs1) 
     else kqs1 ++ (BS.time "check tp" (check_tp me env vv1 t1 lps) x2))
    |> C.group_sol_update s k2s 

(***************************************************************)
(************************* Satisfaction ************************)
(***************************************************************)

let unsat me s c = 
  let env      = C.env_of_t c in
  let (vv,t,_) = C.lhs_of_t c in
  let lps      = C.preds_of_lhs s c  in
  let rhsp     = c |> C.rhs_of_t |> C.preds_of_reft s |> A.pAnd in
  not ((check_tp me env vv t lps [(0, rhsp)]) = [0])

let unsat_constraints me s =
  Ci.to_list me.sri |> List.filter (unsat me s)

(***************************************************************)
(************************ Debugging/Stats **********************)
(***************************************************************)

let key_of_quals qs = 
  qs |> List.map P.to_string 
     |> List.sort compare
     |> String.concat ","

let dump_solution_cluster s = 
   s |> Sy.sm_to_list 
     |> List.map snd 
     |> Misc.groupby key_of_quals
     |> List.map begin fun (ps::_ as pss) -> 
         Co.cprintf Co.ol_solve "SolnCluster: preds %d = size %d \n"
           (List.length ps) (List.length pss)
        end

let print_solution_stats ppf s = 
  let (sum, max, min, bot) =   
    (SM.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
     SM.fold (fun _ qs x -> max x (List.length qs)) s min_int,
     SM.fold (fun _ qs x -> min x (List.length qs)) s max_int,
     SM.fold (fun _ qs x -> x + (if List.exists P.is_contra qs then 1 else 0)) s 0) in
  let avg = (float_of_int sum) /. (float_of_int (Sy.sm_length s)) in
  F.fprintf ppf "# Vars: Total=%d, False=%d \n" (Sy.sm_length s) bot;
  F.fprintf ppf "# Quals: Total=%d, Avg=%f, Max=%d, Min=%d \n" sum avg max min

let print_solver_stats ppf me = 
  let cs   = Ci.to_list me.sri in 
  let cn   = List.length cs in
  let scn  = List.length (List.filter C.is_simple cs) in
  F.fprintf ppf "#constraints = %d \n" cn;
  F.fprintf ppf "#simple constraints = %d \n" scn;
  F.fprintf ppf "#Refine Iterations = %d (si=%d tp=%d unsatLHS=%d) \n"
    !stat_refines !stat_simple_refines !stat_tp_refines !stat_unsatLHS;
  F.fprintf ppf "#Queries: match=%d, ask=%d, valid=%d\n" 
    !stat_matches !stat_imp_queries !stat_valid_queries;
  F.fprintf ppf "%a" TP.print_stats me.tpc;
  F.fprintf ppf "Iteration Frequency: \n";
  hashtbl_print_frequency stat_cfreqt

let dump me s = 
  Co.cprintf Co.ol_solve_stats "%a \n" print_solver_stats me;
  Co.cprintf Co.ol_solve_stats "%a \n" print_solution_stats s;
  dump_solution_cluster s

(***************************************************************)
(******************** Qualifier Instantiation ******************)
(***************************************************************)

let wellformed env q = 
  (* let t    = Q.sort_of_t q in
     let v    = Sy.value_variable t in
     let env' = SM.add v (v,t,[]) env in *)
  A.sortcheck_pred (fun x -> snd3 (SM.find x env)) (Q.pred_of_t q) 

let dupfree_binding xys : bool = 
  let ys  = List.map snd xys in
  let ys' = Misc.sort_and_compact ys in
  List.length ys = List.length ys'

let varmatch_ctr = ref 0

let varmatch (x, y) = 
  let _ = varmatch_ctr += 1 in
  let (x,y) = Misc.map_pair Sy.to_string (x,y) in
  if x.[0] = '@' then
    let x' = Misc.suffix_of_string x 1 in
    Misc.is_prefix x' y
  else true

let valid_binding xys = 
  (dupfree_binding xys) && 
  (List.for_all varmatch xys)

let valid_bindings ys x = 
  ys |> List.map (fun y -> (x,y))
     |> List.filter varmatch 

let inst_qual ys (q : Q.t) : Q.t list =
  let p    = Q.pred_of_t q in
  let t    = Q.sort_of_t q in
  let xs   = p |> P.support                        (* vars of q *)
               |> List.filter Sy.is_wild           (* placevs of q *)
               |> Misc.sort_and_compact in         (* duplicate free placevs *)
  match xs with [] -> [q] | _ ->
    let xyss = List.map (valid_bindings ys) xs     (* candidate bindings *)
               |> Misc.product                     (* generate combinations *) 
               |> List.filter valid_binding        (* remove bogus bindings *)
               |> List.map (List.map (Misc.app_snd A.eVar)) in (* instantiations *)
    let ps'  = List.rev_map (P.substs p) xyss in
    List.map (Q.create None t) ps'

let inst_ext (qs : Q.t list) s wf = 
  let r    = C.reft_of_wf wf in
  let ks   = C.kvars_of_reft r |> List.map snd in
  let s    = List.fold_left (fun s k -> C.sol_add s k [] |> snd) s ks in
  let env  = C.env_of_wf wf in
  let ys   = SM.fold (fun y _ ys -> y::ys) env [] in
  let env' = SM.add (fst3 r) r env in 
  qs |> Misc.flap (inst_qual ys)
     |> Misc.filter (wellformed env')
     |> Misc.map Q.pred_of_t 
     |> Misc.cross_product ks 
     |> C.group_sol_add s ks
     |> snd

let inst wfs qs s =
  (* Co.bprintf mydebug "%a" (Misc.pprint_many true "\n" (C.print_wf None)) wfs; *)
  let rv = List.fold_left (inst_ext qs) s wfs in
  let _ = Printf.printf "varmatch_ctr = %d \n" !varmatch_ctr in
  rv

(***************************************************************)
(******************** Iterative Refinement *********************)
(***************************************************************)

let rec acsolve me w s = 
  let _ = if !stat_refines mod 1000 = 0 
          then Printf.printf "num refines =%d \n" !stat_refines in
  let _ = if Co.ck_olev Co.ol_insane then F.printf "%a" C.print_soln s in
  match Ci.wpop me.sri w with (None,_) -> s | (Some c, w') ->
    let (ch, s')  = BS.time "refine" (refine me s) c in
    let _ = hashtbl_incr_frequency stat_cfreqt (C.id_of_t c) in  
    let _ = Co.bprintf mydebug "iter=%d id=%d ch=%b %a \n" 
            !stat_refines (C.id_of_t c) ch C.print_tag (C.tag_of_t c) in
    let w''       = if ch then Ci.deps me.sri c |> Ci.wpush me.sri w' else w' in 
    acsolve me w'' s' 

(* API *)
let solve me (s : C.soln) = 
  let _  = F.printf "Fixpoint: Validating Initial Solution \n" in
  let _  = BS.time "profile" PP.profile me.sri in
  let s  = if !Co.true_unconstrained then 
             let _ = F.printf "Fixpoint: Pruning unconstrained kvars \n" in
             PP.true_unconstrained s me.sri
           else 
             let _ = F.printf "Fixpoint: NOT Pruning unconstrained kvars \n" in
             s in
  let _  = Co.cprintf Co.ol_insane "%a" Ci.print me.sri;  
           Co.cprintf Co.ol_insane "%a" C.print_soln s;
           dump me s in
  let _  = F.printf "Fixpoint: Initialize Worklist \n" in
  let w  = BS.time "init wkl" Ci.winit me.sri in 
  let s  = BS.time "cleanup"  SM.map (Misc.sort_and_compact) s in
  let _  = F.printf "Fixpoint: Refinement Loop \n" in
  let s  = BS.time "solving"  (acsolve me w) s in
  let _  = dump me s in
  let _  = F.printf "Fixpoint: Testing Solution \n" in
  let u  = BS.time "testing solution" (unsat_constraints me) s in
  let _  = if u != [] then F.printf "Unsatisfied Constraints:\n %a"
                          (Misc.pprint_many true "\n" (C.print_t None)) u in
  (s, u)

(* API *)
let create ts sm ps a ds cs ws qs =
  let s   = BS.time "Qual inst" (inst ws qs) SM.empty in
  let cs' = BS.time "validation" (PP.validate a s) cs in
  let _   = asserts (List.length cs = List.length cs') "Validation fails" in
  let sri = BS.time "Ref index" Ci.create ds cs' in
  let tpc = TP.create ts sm ps in
  ({ tpc = tpc; sri = sri; ws = ws}, s)
 
(* API *)
let save fname me s =
  let oc  = open_out fname in
  let ppf = F.formatter_of_out_channel oc in
  F.fprintf ppf "@[%a@] \n" Ci.print me.sri;
  List.iter (F.fprintf ppf "@[%a@] \n" (C.print_wf None)) me.ws;
  F.fprintf ppf "@[%a@] \n" C.print_soln s;
  close_out oc


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
