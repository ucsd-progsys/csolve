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

(* This module implements basic datatypes and operations on constraints *)
   
type tag          = int
type substitution = (Symbol.t * Expression.t) list                  (* [x,e] *)
type refineatom   = Conc of Predicate.t | Kvar of subs * Symbol.t
type refinement   = Symbol.t * (refineatom list)                    (* VV,... *)

type environment  = (Sort.t * refinement) Symbol.SMap.t
type solution     = refpred list Symbol.SMap.t
type t            = environment * P.t * refinement * refinement * (tag option) 

let to_string = failwith "TBD" 

(*************************************************************)
(******************** Solution Management ********************)
(*************************************************************)

let sol_read s k = 
  try SM.find s k with Not_found -> 
    failure "ERROR: sol_read : unknown kvar %s \n" s

let sol_update s k qs' =
  let qs = sol_read s k in
  (not (Misc.same_length qs qs'), SM.replace s k qs')

let group_sol_update s0 kqs = 
  let t  = Hashtbl.create 17 in
  let _  = List.iter (fun (k,q) -> Hashtbl.add t k q) kqs in
  let ks = Misc.hashtbl_keys t in
  List.fold_left 
    (fun (b, s) k -> 
      let qs       = Hashtbl.find_all t k in 
      let (b', s') = sol_update s k qs in
      (b || b', s'))
    (false, s0) ks

(*************************************************************)
(*********************** Logic Embedding *********************)
(*************************************************************)

let apply_substs xes p = 
  List.fold_left (fun p' (x,e) -> P.subst p' x e) p xes

let refineatom_preds s   = function
  | Conc p       -> [p]
  | Kvar (xes,k) -> List.map (apply_substs xes) (sol_read s k)

let refinement_preds s (_,ras) =
  Misc.flap (refineatom_preds s) ras

let environment_preds s env =
  SM.fold
    (fun x (t, (vv,ras)) ps -> 
      let vps = refinement_preds s (vv, ras) in
      let xps = List.map (fun p -> P.subst p (vv, E.Var x)) vps in
      xps ++ ps)
    [] env

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)
let print_sub ppf (x,e) = 
  Format.fprintf "[%s:=%a]" x E.print e

let print_refineatom ppf = function
  | Conc p        -> Format.fprintf "%a" P.print p
  | Kvar (xes, k) -> Format.fprintf "%s[%a]" k 
                       (Misc.pprint_many false "" print_sub) xes

let print_refinement ppf (v, ras) =
  Format.fprintf ppf "@[{%s:%a@]" v 
    (Misc.pprint_many false " /\ " print_refineatom) ras  

let print_binding ppf (x, (t, r)) = 
  Format.fprintf ppf "@[%s@ =>@ %a:%a@],@;<0 2>" x Sort.print t print_refinement r 

let print_env so ppf env = 
  match so with
  | Some s -> P.print ppf (P.And (environment_preds s env))
  | None   -> SM.iter (fun x y -> print_binding ppf (x, y)) env 

let pprint_io ppf = function
  | Some id -> Format.fprintf ppf "(%d)" id
  | None    -> Format.fprintf ppf "()"

let print so ppf (env,g,r1,r2,io) =
  Format.fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
    pprint_io io 
    (print_env so) env 
    P.print g
    print_refinement r1
    print_refinement r2

(**************************************************************)
(****************** Debug/Profile Information *****************)
(**************************************************************)

(* TODO HEREHEREHEREHERE *)

(* API: dump_solution, dump_solving, dump_constraints *)

let dump_ref_constraints sri =
  if !Co.dump_ref_constraints then begin
    Format.printf "Refinement Constraints \n";
    Ci.iter sri (fun c -> Format.printf "@[%a@.@]" (pprint_ref None) c);
    printf "@[SCC Ranked Refinement Constraints@.@\n@]";
    sort_iter_ref_constraints sri (fun c -> printf "@[%a@.@]" (pprint_ref None) c);
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
    List.iter (fun (c, qs) -> List.iter (fun q -> printf "%a@." Qualifier.pprint q) qs;
                              if not(C.empty_list qs) then printf "@.") cqs;
     printf "done.@.")



