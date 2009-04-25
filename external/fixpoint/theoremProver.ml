(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
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

(* Common theorem prover interface *)
module Co = Constants
module Prover = TheoremProverZ3.Prover
module BS = Bstats
module A = Ast
module P = A.Predicate
module E = A.Expression

(********************************************************************************)
(************************** Rationalizing Division ******************************)
(********************************************************************************)

let rec fixdiv p = 
  let expr_isdiv = 
    function P.Binop(_, P.Div, _) -> true
      | _ -> false in 
  let pull_const =
    function P.PInt(i) -> i
      | _ -> 1 in
  let pull_divisor =
    function P.Binop(_, P.Div, d1) ->
      pull_const d1 
      | _ -> 1 in
  let rec apply_mult m e =
    match e with
        P.Binop(n, P.Div, P.PInt(d)) ->
          (*let _ = assert ((m/d) * d = m) in*)
            P.Binop(P.PInt(m/d), P.Times, n) 
      | P.Binop(e1, rel, e2) ->
          P.Binop(apply_mult m e1, rel, apply_mult m e2) 
      | P.PInt(i) -> P.PInt(i*m)
      | e -> P.Binop(P.PInt(m), P.Times, e)
  in
  let rec pred_isdiv = 
    function P.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
      | P.Iff (p, q) -> pred_isdiv p || pred_isdiv q
      | P.And(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.Or(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.Implies (p, q) -> (pred_isdiv p) || (pred_isdiv q)
      | P.True -> false
      | P.Not p -> pred_isdiv p
      | P.Forall (_, q) | P.Exists (_, q) -> pred_isdiv q 
      | P.Boolexp e -> expr_isdiv e in
  let calc_cm e1 e2 =
    pull_divisor e1 * pull_divisor e2 in
  if pred_isdiv p then
     match p with
       P.Atom(e, r, e') -> 
         let m = calc_cm e e' in
         let e'' = P.Binop(e', P.Minus, P.PInt(1)) in
         let bound (e, r, e', e'') = 
           P.And(P.Atom(apply_mult m e, P.Gt, apply_mult m e''),
                 P.Atom(apply_mult m e, P.Le, apply_mult m e')) in
           (match (e, r, e') with
                (P.Var v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | (P.PInt v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | _ -> p) 
     | P.And(p1, p2) -> 
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.And(p1, p2)      
     | P.Or(p1, p2) ->
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.Or(p1, p2) 
     | P.Implies(p1, p2) ->
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.Implies(p1, p2)
     | P.Not p1 -> P.Not(fixdiv p1) 
     | p -> p
    else p

(********************************************************************************)
(*********************** Memo tables and Stats Counters  ************************)
(********************************************************************************)

let nb_push = ref 0
let nb_queries = ref 0
let nb_cache_misses = ref 0
let nb_cache_hits = ref 0
let nb_qp_miss = ref 0
let qcachet: (string * string, bool) Hashtbl.t = Hashtbl.create 1009 
let buftlhs = Buffer.create 300
let buftrhs = Buffer.create 300
let lhsform = Format.formatter_of_buffer buftlhs
let rhsform = Format.formatter_of_buffer buftrhs
(*let qcachet: (P.t * P.t, bool) Hashtbl.t = Hashtbl.create 1009*)


(********************************************************************************)
(************************************* AXIOMS ***********************************)
(********************************************************************************)

let push_axiom env p =
  C.cprintf C.ol_axioms "@[Pushing@ axiom:@ %a@]@." P.pprint p; Prover.axiom env p

(********************************************************************************)
(************************************* API **************************************)
(********************************************************************************)

(* API *)
let print_stats ppf () =
  C.fcprintf ppf C.ol_solve_stats "@[TP@ API@ stats:@ %d@ pushes@ %d@ queries@ cache@ %d@ hits@ %d@ misses@]@." !nb_push !nb_queries !nb_cache_hits !nb_cache_misses;
  C.fcprintf ppf C.ol_solve_stats "@[Prover @ TP@ stats:@ %a@]@." Prover.print_stats ()

(* API *)
let reset () =
  Hashtbl.clear qcachet; 
  nb_push  := 0;
  nb_queries := 0; 
  nb_cache_misses := 0;
  nb_cache_hits := 0;
  nb_qp_miss := 0

let is_not_taut p =
  not (P.is_taut p)

let set_and_filter env ps qs =
  let _ = incr nb_push in
  let _ = nb_queries := !nb_queries + (List.length ps) in
  let ps = List.rev_map fixdiv ps in
  (*let ps = BS.time "TP taut" (List.filter is_not_taut) ps in*)
  if BS.time "TP set" (Prover.set env) ps then (Prover.finish (); qs) else
      let qs = List.rev_map (C.app_snd fixdiv) qs in
      let (qs, qs') = List.partition (fun (_, q) -> is_not_taut q) qs in
        List.rev_append qs' (BS.time "TP filter" (Prover.filter env) qs)



