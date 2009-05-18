(**************************************************************)
(**************** Type definitions: Constraints ***************) 
(**************************************************************)

type fc_id = int option 

type subref_id = int 

module SIM = Map.Make(struct type t = subref_id let compare = compare end)

type guard_t = (Path.t * bool) list

type frame_constraint =
  | SubFrame of F.t Le.t * guard_t * F.t * F.t
  | WFFrame of F.t Le.t * F.t

type labeled_constraint = {
  lc_cstr: frame_constraint;
  lc_tenv: Env.t;
  lc_orig: origin;
  lc_id: fc_id;
}

and origin =
  | Loc of Location.t 
  | Assert of Location.t 
  | Cstr of labeled_constraint

type refinement_constraint =
  | SubRef of F.refinement (* F.t *) Le.t * guard_t * F.refinement * F.simple_refinement * (subref_id option)
  | WFRef of F.t Le.t * F.simple_refinement * (subref_id option)

(**************************************************************)
(********************** Misc. Constants ***********************)
(**************************************************************)

let fresh_fc_id = 
  let r = ref 0 in
  fun () -> incr r; Some (!r)

(* Unique variable to qualify when testing sat, applicability of qualifiers...
 * this is passed into the solver *)
let qual_test_var = C.qual_test_var(*Path.mk_ident "AA"*)
let qual_test_expr = P.Var qual_test_var

let is_simple_constraint c = match c with 
  | SubRef (_, _, r1, ([], F.Qvar _), _) ->
      List.for_all (function ([], ([], _)) -> true | _ -> false) r1
  | _ -> false

let is_simple_constraint2 = function 
  | SubRef (_, _, [([], ([], [k1]))], ([], F.Qvar k2), _) -> true
  | _ -> false

let is_subref_constraint = function 
  SubRef _ -> true | _ -> false

let is_wfref_constraint = function 
  WFRef _ -> true | _ -> false

let is_subframe_constraint = function
  SubFrame _ -> true | _ -> false

let is_wfframe_constraint = function
  WFFrame _ -> true | _ -> false

let solution_map s k = 
  C.do_catch 
    (Printf.sprintf "ERROR: solution_map couldn't find: %s" (C.path_name k))
    (Sol.find s) k  

let sref_map f r =
  let (qconsts, qvars) = F.ref_to_simples r in 
  List.map f (qconsts @ qvars)

(**************************************************************)
(**************************** Stats ***************************)
(**************************************************************)

let stat_unsat_lhs      = ref 0
let stat_wf_refines     = ref 0
let stat_sub_refines    = ref 0
let stat_simple_refines = ref 0
let stat_refines        = ref 0
let stat_imp_queries    = ref 0
let stat_valid_queries  = ref 0
let stat_matches        = ref 0
let stat_tp_refines     = ref 0

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let guard_predicate () g = 
  P.big_and 
    (List.map 
      (fun (v,b) -> let p = P.(?.) (P.Var v) in 
         if b then p else P.Not p) 
      g)

let refinement_preds sm qexpr r =
  F.refinement_conjuncts sm qexpr r

let environment_preds sm env = 
  List.flatten (Le.maplist (fun v r -> refinement_preds sm (P.Var v) r) env)

let pprint_local_binding f ppf = function
  | (Path.Pident _ as k, v) -> 
      fprintf ppf "@[%s@ =>@ %a@],@;<0 2>" 
      (Path.unique_name k) f v
  | _ -> ()

let env_ignore_list = 
  ["Pervasives"; "Open_"; "FP_"; "false"; "true"; "Array"; "String"; "Big"; "None"; "Some"; "Random"; "[]"; "::"]

let filter_le f e = Le.fold (fun p fr m -> if f p fr then Le.add p fr m else m) e Le.empty

let prune_background env = 
  filter_le (fun p _ -> List.for_all (fun pre -> not (C.has_prefix pre (C.path_name p))) env_ignore_list) env 

let pprint_fenv ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint f) (prune_background env); fprintf ppf "==="

let pprint_fenv_shp ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint (F.shape f)) (prune_background env); fprintf ppf "==="

let pprint_fenv_pred so ppf env =
  (* match so with
  | Some s -> P.pprint ppf (P.big_and (environment_preds (solution_map s) env))
  | _ -> *) Le.iter (fun x t -> pprint_local_binding F.pprint ppf (x, t)) env

let pprint_renv_pred f so ppf env =
  match so with
  | Some s -> P.pprint ppf (P.big_and (environment_preds (solution_map s) env))
  | _ -> Le.iter (fun x t -> pprint_local_binding F.pprint_refinement ppf (x, t)) env

let pprint ppf = function
  | SubFrame (e,g,f1,f2) ->
      if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Env)@.%a@]@." pprint_fenv e;
      if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Guard)@.%a@]@.@." P.pprint (guard_predicate () g);
      fprintf ppf "@[%a@ <:@;<1 2>%a@]" F.pprint f1 F.pprint f2
  | WFFrame (e,f) ->
      if C.ck_olev C.ol_dump_wfs then begin
        if C.ck_olev C.ol_verb_constrs then fprintf ppf "@[(Env)@.%a@]@." pprint_fenv e;
        fprintf ppf "@[|- %a@]@." F.pprint f
      end

let pprint_io ppf = function
  | Some id -> fprintf ppf "(%d)" id
  | None    -> fprintf ppf "()"

let pprint_ref so ppf = 
  function
  | SubRef (renv,g,r1,sr2,io) ->
      let renv = prune_background renv in
      fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
      pprint_io io (pprint_renv_pred F.pprint_refinement so) renv 
      P.pprint (guard_predicate () g) 
      F.pprint_refinement r1 F.pprint_refinement (F.ref_of_simple sr2)
  | WFRef (env,sr,io) ->
      let env = prune_background env in
      C.fcprintf ppf C.ol_dump_wfs "@[%a@ Env:@ @[%a@];@\n|-@;<1 2>%a@;<1 2>@]"
      pprint_io io (pprint_fenv_pred so) env 
      F.pprint_refinement (F.ref_of_simple sr)

let pprint_orig ppf = function
  | Loc l -> fprintf ppf "Loc@ %a" Location.print l
  | Assert l -> fprintf ppf "Assert@ %a" Location.print l
  | Cstr l -> match l.lc_id with Some l -> fprintf ppf "->@ %i" l | None -> fprintf ppf "->@ ?"

(**************************************************************)
(************************** Refinement ************************)
(**************************************************************)

module PM = Map.Make(struct type t = P.t let compare = compare end)

let refine_sol_update s k qs qs' = 
  BS.time "sol replace" (Sol.replace s k) qs';
  not (C.same_length qs qs')

let refine_simple s r1 k2 =
  let k1s  = C.flap (fun (_,(_,ks)) -> ks) r1 in
  let q1s  = C.flap (Sol.find s) k1s in
  let q1s  = List.fold_left (fun qs q -> QSet.add q qs) QSet.empty q1s in
  let q2s  = Sol.find s k2 in
  let q2s' = List.filter (fun q -> QSet.mem q q1s) q2s in
  refine_sol_update s k2 q2s q2s'

let qual_wf sm env subs q =
  BS.time "qual_wf" 
  (refinement_well_formed env sm (F.mk_refinement subs [q] [])) qual_test_expr

let lhs_preds sm env g r1 =
  let gp    = guard_predicate () g in
  let envps = environment_preds sm env in
  let r1ps  = refinement_preds  sm qual_test_expr r1 in
  envps @ (gp :: r1ps) 

let rhs_cands sm subs k = 
  List.map 
    (fun q -> 
      let r = F.mk_refinement subs [q] [] in
      (q,F.refinement_predicate sm qual_test_expr r))
    (sm k) 

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

let bound_in_env senv p =
  List.for_all (fun x -> Le.mem x senv) (P.vars p)

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
      (*let x2      = List.filter (fun (_,p) -> bound_in_env senv p) x2 in*)
      match x2 with [] -> x1 | _ -> x1 @ (check_tp senv lhs_ps x2) in
  refine_sol_update s k2 rhs_qps (List.map fst rhs_qps') 

let refine sri s c =
  (incr stat_refines;
   match c with 
   | SubRef _ -> incr stat_sub_refines 
   | WFRef _ -> incr stat_wf_refines); 
  let sm = solution_map s in
  match c with
  | SubRef (_, _, r1, ([], F.Qvar k2), _)
    when is_simple_constraint c && not (!Cf.no_simple || !Cf.verify_simple) ->
      incr stat_simple_refines; 
      refine_simple s r1 k2
  | SubRef (_, _, _, (_, F.Qvar k2), _) when C.empty_list (sm k2) ->
      false
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      refine_tp (get_ref_fenv sri c) s env g r1 sub2s k2 
  | WFRef (env, (subs, F.Qvar k), Some id) ->
      let qs  = solution_map s k in
      let _   = if C.ck_olev C.ol_dump_wfs then printf "@.@.@[WF: %s@]@." (Path.unique_name k) in
      let _   = if C.ck_olev C.ol_dump_wfs then printf "@[(Env)@ %a@]@." pprint_fenv_shp env in 
      let qs' = BS.time "filter wf" (List.filter (qual_wf sm env subs)) qs in
      let _   = if C.ck_olev C.ol_dump_wfs then List.iter (fun q -> printf "%a" Qualifier.pprint q) qs in
      let _   = if C.ck_olev C.ol_dump_wfs then printf "@.@." in
      let _   = if C.ck_olev C.ol_dump_wfs then List.iter (fun q -> printf "%a" Qualifier.pprint q) qs' in
      refine_sol_update s k qs qs'
  | _ -> false

(**************************************************************)
(********************** Constraint Satisfaction ***************)
(**************************************************************)

let sat sri s c = 
  match c with
  | SubRef (env,g,r1, (sub2s, F.Qvar k2), _)  ->
      not (refine_tp (get_ref_fenv sri c) s env g r1 sub2s k2)
  | SubRef (env, g, r1, sr2, _) as c ->
      let sm     = solution_map s in
      let lhs_ps = lhs_preds sm env g r1 in
      let rhs    = F.refinement_predicate sm qual_test_expr (F.ref_of_simple sr2) in
      1 = List.length (check_tp (get_ref_fenv sri c) lhs_ps [(0,rhs)])
  | WFRef (env,(subs, F.Qvar k), _) ->
      true 
  | _ -> true

let unsat_constraints sri s =
  C.map_partial
    (fun c -> if sat sri s c then None else Some (c, get_ref_orig sri c))
    (get_ref_constraints sri)

(**************************************************************)
(************************ Initial Solution ********************)
(**************************************************************)

(* If a variable only ever appears on the left hand side, the variable is
 * unconstrained; this generally indicates an uncalled function.
 * When we have such an unconstrained variable, we simply say we don't
 * know anything about its value.  For uncalled functions, this will give
 * us the type which makes the least assumptions about the input. *)

let strip_origins cs = snd (List.split cs)

let formals = ref []
let is_formal q = List.mem q !formals
let formals_addn qs = formals := List.rev_append qs !formals

let filter_wfs cs = List.filter (fun (r, _) -> match r with WFRef(_, _, _) -> true | _ -> false) cs
let filter_subs cs = List.filter (fun (r, _) -> match r with SubRef(_, _, _, _, _) -> true | _ -> false) cs
type solmode = WFS | LHS | RHS

let app_sol s s' l k qs = 
  let f qs q = QSet.add q qs in
  if Sol.mem s k then
    if Sol.mem s' k then
      Sol.replace s' k (List.fold_left f (Sol.find s' k) qs)
    else if !Clflags.union_wfs then (l := k :: !l; Sol.replace s' k (List.fold_left f (List.fold_left f QSet.empty (Sol.find s k)) qs))
  else Sol.replace s k qs

let make_initial_solution cs =
  let srhs = Hashtbl.create 100 in
  let slhs = Hashtbl.create 100 in
  let s = Sol.create 100 in
  let s' = Sol.create 100 in
  let l = ref [] in
  let _ = List.iter (function (SubRef (_, _, _, (_, F.Qvar k), _), qs) ->
            let k' = Path.unique_ident_name_crash k in
            (Hashtbl.replace srhs k' (); Sol.replace s k qs) | _ -> ()) cs in
  let _ = List.iter (function (SubRef (_, _, r1, _, _), qs) ->
            List.iter (fun k ->
              let k' = Path.unique_ident_name_crash k in
              Hashtbl.replace slhs k' (); if not !Cf.minsol && is_formal k && not (Hashtbl.mem srhs k')
              then Sol.replace s k [] else Sol.replace s k qs) (F.refinement_qvars r1) | _ -> ()) cs in
  let _ = List.iter (function (WFRef (_, (_, F.Qvar k), _), qs) ->
            let k' = Path.unique_ident_name_crash k in
            if Hashtbl.mem srhs k' || (Hashtbl.mem slhs k' && Sol.find s k != []) then BS.time "app_sol" (app_sol s s' l) k qs else Sol.replace s k [] | _ -> ()) cs in
  let l = BS.time "sort and compact" C.sort_and_compact !l in
  let _ = BS.time "elements" (List.iter (fun k -> Sol.replace s k (QSet.elements (Sol.find s' k)))) l in
  s

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)
 
let dump_ref_constraints sri =
  if !Cf.dump_ref_constraints then begin
    printf "@[Refinement Constraints@.@\n@]";
    iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c);
    printf "@[SCC Ranked Refinement Constraints@.@\n@]";
    sort_iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c);
    (*printf "@[Refinement Constraint Origins@.@.@]";
    iter_ref_origs sri 
      (fun i c -> printf "@[o(%i)@]@." i; Le.iter (fun p _ -> printf "@[%s@]@." (Path.unique_name p)) (frame_env c.lc_cstr))*)  
  end

let dump_ref_vars sri =
  if !Cf.dump_ref_vars then
  (printf "@[Refinement Constraint Vars@.@\n@]";
  iter_ref_constraints sri (fun c -> printf "@[(%d)@ %s@.@]" (ref_id c) 
    (match (ref_k c) with Some k -> Path.unique_name k | None -> "None")))
   
let dump_constraints cs =
  if !Cf.dump_constraints then begin
    printf "******************Frame Constraints****************@.@.";
    let index = ref 0 in
    List.iter (fun {lc_cstr = c; lc_orig = d} -> if (not (is_wfframe_constraint c)) || C.ck_olev C.ol_dump_wfs then 
            (incr index; printf "@[(%d)(%a) %a@]@.@." !index pprint_orig d pprint c)) cs;
    printf "@[*************************************************@]@.@.";
  end

let dump_solution_stats s = 
  if C.ck_olev C.ol_solve_stats then
    let kn  = Sol.length s in
    let (sum, max, min) =   
      (Sol.fold (fun _ qs x -> (+) x (List.length qs)) s 0,
      Sol.fold (fun _ qs x -> max x (List.length qs)) s min_int,
      Sol.fold (fun _ qs x -> min x (List.length qs)) s max_int) in
    C.cprintf C.ol_solve_stats "@[Quals:@\n\tTotal:@ %d@\n\tAvg:@ %f@\n\tMax:@ %d@\n\tMin:@ %d@\n@\n@]"
    sum ((float_of_int sum) /. (float_of_int kn)) max min;
    print_flush ()
  else ()
  
let dump_unsplit cs =
  let cs = if C.ck_olev C.ol_solve_stats then List.rev_map (fun c -> c.lc_cstr) cs else [] in
  let cc f = List.length (List.filter f cs) in
  let (wf, sub) = (cc is_wfframe_constraint, cc is_subframe_constraint) in
  C.cprintf C.ol_solve_stats "@.@[unsplit@ constraints:@ %d@ total@ %d@ wf@ %d@ sub@]@.@." (List.length cs) wf sub

let dump_solving sri s step =
  if step = 0 then 
    let cs   = get_ref_constraints sri in 
    let kn   = Sol.length s in
    let wcn  = List.length (List.filter is_wfref_constraint cs) in
    let rcn  = List.length (List.filter is_subref_constraint cs) in
    let scn  = List.length (List.filter is_simple_constraint cs) in
    let scn2 = List.length (List.filter is_simple_constraint2 cs) in
    (dump_ref_vars sri;
     dump_ref_constraints sri;
     C.cprintf C.ol_solve_stats "@[%d@ variables@\n@\n@]" kn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ wf@ constraints@\n@\n@]" wcn;
     C.cprintf C.ol_solve_stats "@[%d@ split@ subtyping@ constraints@\n@\n@]" rcn;
     C.cprintf C.ol_solve_stats "@[%d@ simple@ subtyping@ constraints@\n@\n@]" scn;
     C.cprintf C.ol_solve_stats "@[%d@ simple2@ subtyping@ constraints@\n@\n@]" scn2;
     dump_solution_stats s) 
  else if step = 1 then
    dump_solution_stats s
  else if step = 2 then
    (C.cprintf C.ol_solve_stats 
      "@[Refine Iterations: %d@ total (= wf=%d + su=%d) sub includes si=%d tp=%d unsatLHS=%d)\n@\n@]"
      !stat_refines !stat_wf_refines  !stat_sub_refines !stat_simple_refines !stat_tp_refines !stat_unsat_lhs;
     C.cprintf C.ol_solve_stats "@[Implication Queries:@ %d@ match;@ %d@ to@ TP@ (%d@ valid)@]@.@." 
       !stat_matches !stat_imp_queries !stat_valid_queries;
     if C.ck_olev C.ol_solve_stats then TP.print_stats std_formatter () else ();
     dump_solution_stats s;
     flush stdout)

let dump_solution s =
  if C.ck_olev C.ol_solve then
    Sol.iter (fun p r -> C.cprintf C.ol_solve "@[%s: %a@]@."
              (Path.unique_name p) (Oprint.print_list Q.pprint C.space) r) s
  else ()

let dump_qualifiers cqs =
  if C.ck_olev C.ol_insane then
    (printf "Raw@ generated@ qualifiers:@.";
    List.iter (fun (c, qs) -> printf "%a: " (pprint_ref None) c;
                              List.iter (fun q -> printf "%a " Qualifier.pprint q) qs;
                              printf "@.@.") cqs)

(**************************************************************)
(******************** Iterative - Refinement  *****************)
(**************************************************************)

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

let solve qs cs = 
  let cs = if !Cf.simpguard then List.map simplify_fc cs else cs in
  let _  = dump_constraints cs in
  let _  = dump_unsplit cs in
  let cs = BS.time "splitting constraints" split cs in
  let max_env = List.fold_left 
    (fun env (v, c, _) -> Le.combine (frame_env c.lc_cstr) env) Le.empty cs in
  let cs = List.map (fun (v, c, cstr) -> (set_labeled_constraint c (make_val_env v max_env), cstr)) cs in
  let qs = BS.time "instantiating quals" (instantiate_per_environment cs) qs in
  let _ = C.cprintf C.ol_solve "@[%i@ instantiation@ queries@ %i@ misses@]@." (List.length cs) !tr_misses in
  let _ = dump_qualifiers (List.combine (strip_origins cs) qs) in
  let sri = BS.time "making ref index" make_ref_index cs in
  let s = BS.time "make initial sol" make_initial_solution (List.combine (strip_origins cs) qs) in
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
  (if List.length unsat > 0 then 
    C.cprintf C.ol_solve_error "@[Ref_constraints@ still@ unsatisfied:@\n@]";
    List.iter (fun (c, b) -> C.cprintf C.ol_solve_error "@[%a@.@\n@]" (pprint_ref None) c) unsat);
  (solution_map s, List.map snd unsat)
