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
module C  = Ast.Constraint

(* JHALA: What the hell is this ? *)

type t = int * TP.t

let make =
  let ct = ref 0 in
  (fun () -> if c > 0 then None else incr c; Some (1, TP.t))


(*************************************************************)
(********************* Stats *********************************)
(*************************************************************)

let stat_refines = ref 0
let stat_simple_refines = ref 0

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

let apply_substs xes p = 
  List.fold_left (fun p' (x,e) -> P.subst p' x e) p xes

let refine_sol_read s k = 
  try SM.find s k with Not_found -> 
    failure "ERROR: refine_sol_read : unknown kvar %s \n" s

let refine_sol_update s k qs' =
  let qs = refine_sol_read s k in
  (not (Misc.same_length qs qs'), SM.replace s k qs')

let refineatom_preds s   = function
  | Conc p       -> [p]
  | Kvar (xes,k) -> List.map (apply_substs xes) (refine_sol_read s k)

let refinement_preds s (_,ras) =
  Misc.flap (refineatom_preds s) ras

let environment_preds s env =
  SM.fold
    (fun x (t, (vv,ras)) ps -> 
      let vps = refinement_preds s (vv, ras) in
      let xps = List.map (fun p -> P.subst p (vv, E.Var x)) vps in
      xps ++ ps)
    [] env

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
  envps ++ (gp :: r1ps) 

let rhs_cands s = function
  | C.Kvar (xes, k) -> 
      refine_sol_read s k |>
      List.map (fun q -> ((k,q), apply_substs xes q))
  | _ -> []

(* JUNK BEGIN {{{ *)
let check_tp senv lhs_ps x2 = 
  let dump s p = 
    let p = List.map (fun p -> P.big_and (List.filter (function P.Atom(p, P.Eq, p') when p = p' -> false | _ -> true) (P.conjuncts p))) p in
    let p = List.filter (function P.True -> false | _ -> true) p in
    C.cprintf C.ol_dump_prover "@[%s:@ %a@]@." s (C.pprint_many false " " P.pprint) p in
  let dump s p = if C.ck_olev C.ol_dump_prover then dump s p else () in
  let _ = dump "Assert" lhs_ps in
  let _ = if C.ck_olev C.ol_dump_prover then dump "Ck" (snd (List.split x2)) in
  let rv = 
    try
      TP.set_and_filter senv lhs_ps x2
    with Failure x -> printf "%a@." pprint_fenv senv; raise (Failure x) in
  let _ = if C.ck_olev C.ol_dump_prover then dump "OK" (snd (List.split rv)) in
  incr stat_tp_refines;
  stat_imp_queries   := !stat_imp_queries + (List.length x2);
  stat_valid_queries := !stat_valid_queries + (List.length rv); rv

let check_tp senv lhs_ps x2 =
  if C.empty_list x2 then (incr stat_tp_refines; []) else BS.time "check_tp" (check_tp senv lhs_ps) x2 

let refine_tp senv s env g r1 sub2s k2 =
  let sm = solution_map s in
  let lhs_ps  = lhs_preds sm env g r1 in
  let rhs_qps = rhs_cands sm sub2s k2 in
  let rhs_qps' =
    if List.exists P.is_contra lhs_ps 
    then (stat_matches := !stat_matches + (List.length rhs_qps); rhs_qps) 
    else
      let rhs_qps = List.filter (fun (_,p) -> not (P.is_contra p)) rhs_qps in
      let lhsm    = List.fold_left (fun pm p -> PM.add p true pm) PM.empty lhs_ps in
      let (x1,x2) = List.partition (fun (_,p) -> PM.mem p lhsm) rhs_qps in
      let _       = stat_matches := !stat_matches + (List.length x1) in 
      match x2 with [] -> x1 | _ -> x1 @ (check_tp senv lhs_ps x2) in
  refine_sol_update s k2 rhs_qps (List.map fst rhs_qps') 


  (* JUNK END }}} *)

let check_tp env lps rcs =

let group_and_update s0 rcs = 
  let t  = Hashtbl.create 17 in
  let _  = List.iter (fun (k,q) -> Hashtbl.add t k q) rcs in
  let ks = Misc.hashtbl_keys t in
  List.fold_left 
    (fun (b, s) k -> 
      let qs       = Hashtbl.find_all t k in 
      let (b', s') = refine_sol_update s k qs in
      (b || b', s'))
    (false, s0) ks

let refine s ((env, g, (vv1, ra1s), (vv2, ra2s), _) as c) =
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
     else kqs1 ++ (check_tp env lps x2))
    |> group_and_update s 

(***************************************************************)
(************************* Satisfaction ************************)
(***************************************************************)

let sat sri s c = 
  match c with
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      true
  | SubRef (env, g, r1, sr2, _) as c ->
      let sm     = solution_map s in
      let lhs_ps = lhs_preds sm env g r1 in
      let rhs    = F.refinement_predicate sm qual_test_expr (F.ref_of_simple sr2) in
      (1 = List.length (check_tp (get_ref_fenv sri c) lhs_ps [(0,rhs)]))
  | WFRef (env,(subs, F.Qvar k), _) ->
      true 
  | _ -> true

let unsat_constraints sri s =
  C.map_partial
    (fun c -> if sat sri s c then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(***************************************************************)
(*************************** Indexing **************************)
(***************************************************************)
(* TBD BREAK THIS OUT INTO ITS OWN MODULE *)

module WH = 
  Heap.Functional(struct 
      type t = subref_id * int * (int * bool * fc_id)
      let compare (_,ts,(i,j,k)) (_,ts',(i',j',k')) =
        if i <> i' then compare i i' else
          if ts <> ts' then -(compare ts ts') else
            if j <> j' then compare j j' else 
              compare k' k
    end)

type ref_index = 
  { orig: labeled_constraint SIM.t;     (* id -> orig *)
    cnst: refinement_constraint SIM.t;  (* id -> refinement_constraint *) 
    rank: (int * bool * fc_id) SIM.t;   (* id -> dependency rank *)
    depm: subref_id list SIM.t;         (* id -> successor ids *)
    pend: (subref_id,unit) Hashtbl.t;   (* id -> is in wkl ? *)
  }

let get_ref_id = 
  function WFRef (_,_,Some i) | SubRef (_,_,_,_,Some i) -> i | _ -> assert false

let get_ref_rank sri c = 
  try SIM.find (get_ref_id c) sri.rank with Not_found ->
    (printf "ERROR: @[No@ rank@ for:@ %a@\n@]" (pprint_ref None) c; 
     raise Not_found)

let get_ref_constraint sri i = 
  C.do_catch "ERROR: get_constraint" (SIM.find i) sri.cnst

let lhs_ks = function WFRef _ -> assert false | SubRef (env,_,r,_,_) ->
  Le.fold (fun _ f l -> F.refinement_qvars f @ l) env (F.refinement_qvars r)

let rhs_k = function
  | SubRef (_,_,_,(_, F.Qvar k),_) -> Some k
  | _ -> None

let wf_k = function
  | WFRef (_, (_, F.Qvar k), _) -> Some k
  | _ -> None

let ref_k c =
  match (rhs_k c, wf_k c) with
  | (Some k, None)
  | (None, Some k) -> Some k
  | _ -> None

let ref_id = function
  | WFRef (_, (_, _), Some id)
  | SubRef (_,_,_,(_, _), Some id) -> id
  | _ -> -1

let print_scc_edge rm (u,v) = 
  let (scc_u,_,_) = SIM.find u rm in
  let (scc_v,_,_) = SIM.find v rm in
  let tag = if scc_u > scc_v then "entry" else "inner" in
  C.cprintf C.ol_solve "@[SCC@ edge@ %d@ (%s)@ %d@ ====> %d@\n@]" scc_v tag u v

let make_rank_map om cm =
  let get vm k = try VM.find k vm with Not_found -> [] in
  let upd id vm k = VM.add k (id::(get vm k)) vm in
  let km = 
    BS.time "step 1"
    (SIM.fold 
      (fun id c vm -> match c with WFRef _ -> vm 
        | SubRef _ -> List.fold_left (upd id) vm (lhs_ks c))
      cm) VM.empty in
  let (dm,deps) = 
    BS.time "step 2"
    (SIM.fold
      (fun id c (dm,deps) -> 
        match (c, rhs_k c) with 
        | (WFRef _,_) -> (dm,deps) 
        | (_,None) -> (dm,(id,id)::deps) 
        | (_,Some k) -> 
          let kds = get km k in
          let deps' = List.map (fun id' -> (id,id')) (id::kds) in
          (SIM.add id kds dm, (List.rev_append deps' deps)))
      cm) (SIM.empty,[]) in
  let flabel i = C.io_to_string ((SIM.find i om).lc_id) in
  let rm = 
    let rank = BS.time "scc rank" (C.scc_rank flabel) deps in
    BS.time "step 2"
    (List.fold_left
      (fun rm (id,r) -> 
        let b = (not !Cf.psimple) || (is_simple_constraint (SIM.find id cm)) in
        let fci = (SIM.find id om).lc_id in
        SIM.add id (r,b,fci) rm)
      SIM.empty) rank in
  (dm,rm)

let fresh_refc = 
  let i = ref 0 in
  fun c -> 
    let i' = incr i; !i in
    match c with  
    | WFRef (env,r,None) -> WFRef (env,r,Some i')
    | SubRef (env,g,r1,r2,None) -> SubRef (env,g,r1,r2,Some i')
    | _ -> assert false

(* API *)
let make_ref_index ocs = 
  let ics = List.map (fun (o,c) -> (o,fresh_refc c)) ocs in
  let (om,cm) = 
    List.fold_left 
      (fun (om,cm) (o,c) ->
        let i = get_ref_id c in 
        (SIM.add i o om, SIM.add i c cm))
      (SIM.empty, SIM.empty) ics in
  let (dm,rm) = BS.time "make rank map" (make_rank_map om) cm in
  {orig = om; cnst = cm; rank = rm; depm = dm; pend = Hashtbl.create 17}

let get_ref_orig sri c = 
  C.do_catch "ERROR: get_ref_orig" (SIM.find (get_ref_id c)) sri.orig

let get_ref_fenv sri c =
  (function SubFrame (a, _, _, _) | WFFrame (a, _) -> a) (get_ref_orig sri c).lc_cstr

(* API *)
let get_ref_deps sri c =
  let is' = try SIM.find (get_ref_id c) sri.depm with Not_found -> [] in
  List.map (get_ref_constraint sri) is'

(* API *)
let get_ref_constraints sri = 
  SIM.fold (fun _ c cs -> c::cs) sri.cnst [] 

(* API *)
let iter_ref_constraints sri f = 
  SIM.iter (fun _ c -> f c) sri.cnst

let iter_ref_origs sri f =
  SIM.iter (fun i c -> f i c) sri.orig

let sort_iter_ref_constraints sri f = 
  let rids  = SIM.fold (fun id (r,_,_) ac -> (id,r)::ac) sri.rank [] in
  let rids' = List.sort (fun x y -> compare (snd x) (snd y)) rids in 
  List.iter (fun (id,_) -> f (SIM.find id sri.cnst)) rids' 

(* API *)
let push_worklist =
  let timestamp = ref 0 in
  fun sri w cs ->
    incr timestamp;
    List.fold_left 
      (fun w c -> 
        let id = get_ref_id c in
        if Hashtbl.mem sri.pend id then w else 
          (C.cprintf C.ol_solve "@[Pushing@ %d at %d@\n@]" id !timestamp; 
           Hashtbl.replace sri.pend id (); 
           WH.add (id,!timestamp,get_ref_rank sri c) w))
      w cs

(* API *)
let pop_worklist sri w =
  try 
    let (id, _, _) = WH.maximum w in
    let _ = Hashtbl.remove sri.pend id in
    (Some (get_ref_constraint sri id), WH.remove w)
  with Heap.EmptyHeap -> (None,w) 

(* API *)
let make_initial_worklist sri =
  let cs = List.filter is_subref_constraint (get_ref_constraints sri) in
  push_worklist sri WH.empty cs 


(***************************************************************)
(******************** Iterative Refinement *********************)
(***************************************************************)

let rec solve_sub sri s w = 
  (if !stat_refines mod 100 = 0 
   then C.cprintf C.ol_solve "@[num@ refines@ =@ %d@\n@]" !stat_refines);
  match pop_worklist sri w with (None,_) -> s | (Some c, w') ->
    let (r,b,fci) = get_ref_rank sri c in
    let _ = C.cprintf C.ol_solve "@.@[Refining@ %d@ at iter@ %d in@ scc@ (%d,%b,%s):@]@."
            (get_ref_id c) !stat_refines r b (C.io_to_string fci) in
    let _ = if C.ck_olev C.ol_insane then dump_solution s in
    let w' = if BS.time "refine" (refine sri s) c 
             then push_worklist sri w' (get_ref_deps sri c) else w' in
    solve_sub sri s w'

let solve_wf sri s =
  iter_ref_constraints sri 
  (function WFRef _ as c -> ignore (refine sri s c) | _ -> ())

let solve qs env consts cs = 
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let _  = dump_constraints cs in
  let _  = dump_unsplit cs in
  let cs = BS.time "splitting constraints" split cs in
  let max_env = List.fold_left 
    (fun env (v, c, _) -> Le.combine (frame_env c.lc_cstr) env) Le.empty cs in
(*  let _ = C.cprintf C.ol_insane "===@.Pruned Maximum Environment@.%a@.===@." pprint_fenv_shp max_env in
  let _ = printf "%a@.@." (pprint_raw_fenv true) max_env; assert false in*)
  let cs = List.map (fun (v, c, cstr) -> (set_labeled_constraint c (make_val_env v max_env), cstr)) cs in
  (* let cs = if !Cf.esimple then 
               BS.time "e-simplification" (List.map esimple) cs else cs in *)
  (*let _ = printf "Qualifier@ patterns@.";
    List.map (fun (_, {Parsetree.pqual_pat_desc = (_, _, p)}) -> printf "%a@." P.pprint_pattern p) qs in*)
  let qs = BS.time "instantiating quals" (instantiate_per_environment env consts cs) qs in
  (*let qs = List.map (fun qs -> List.filter Qualifier.may_not_be_tautology qs) qs in*)
  let _ = if C.ck_olev C.ol_solve then
          C.cprintf C.ol_solve "@[%i@ instantiation@ queries@ %i@ misses@]@." (List.length cs) !tr_misses in
  let _ = if C.ck_olev C.ol_solve then
          C.cprintf C.ol_solve_stats "@[%i@ qualifiers@ generated@]@." (List.length (List.flatten qs)) in
  let _ = if C.ck_olev C.ol_insane then
          dump_qualifiers (List.combine (strip_origins cs) qs) in
(*let _ = assert false in*)
  let sri = BS.time "making ref index" make_ref_index cs in
  let s = BS.time "make initial sol" make_initial_solution (List.combine (strip_origins cs) qs) in
  (*let _ = JS.print stdout "JanStats"; flush stdout in*)

  let _ = dump_solution s in
  let _ = dump_solving sri s 0 in
  let _ = BS.time "solving wfs" (solve_wf sri) s in
  let _ = C.cprintf C.ol_solve "@[AFTER@ WF@]@." in
  let _ = dump_solving sri s 1 in
  let _ = dump_solution s in
  let w = make_initial_worklist sri in
  let _ = BS.time "solving sub" (solve_sub sri s) w in
  let _ = dump_solving sri s 2 in
  let _ = dump_solution s in
  let _ = TP.reset () in
  let unsat = BS.time "testing solution" (unsat_constraints sri) s in
  (*let sat = List.for_all (fun x -> x) (BS.time "testing solution" (List.map (refine sri s)) cs') in*)
  if List.length unsat > 0 then 
    C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
    List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat;
  (solution_map s, List.map snd unsat)

