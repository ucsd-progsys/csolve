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
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONSy.
 *
 *)

(* This module implements basic datatypes and operations on constraints *)

module A  = Ast
module E  = A.Expression
module P  = A.Predicate
module Sy = A.Symbol
module SM = Sy.SMap

open Misc.Ops

type tag  = int
type subs = (Sy.t * A.expr) list                         (* [x,e] *)
type refa = Conc of A.pred | Kvar of subs * Sy.t
type reft = Sy.t * A.Sort.t * (refa list)                (* { VV: t | [ra] } *)
type envt = reft SM.t
type soln = A.pred list SM.t
type t    = envt * A.pred * reft * reft * (tag option) 
type deft = Srt of Ast.Sort.t 
          | Axm of Ast.pred 
          | Cst of t 
          | Sol of Ast.Symbol.t * Ast.pred list


(*************************************************************)
(************************** Random ***************************)
(*************************************************************)

let get_id = function 
  | (_,_,_,_,Some i) -> i 
  | _                -> failure "MATCH FAILURE: Constraint.get_id"

let is_simple_refatom = function 
  | Kvar ([], _) -> true
  | _            -> false

(* API *)
let is_simple (_,_,(_,_,ra1s),(_,_,ra2s),_) = 
  List.for_all is_simple_refatom ra1s &&
  List.for_all is_simple_refatom ra2s &&
  not (!Constants.no_simple || !Constants.verify_simple)

(*************************************************************)
(******************** Solution Management ********************)
(*************************************************************)

(* API *)
let sol_read s k = 
  try SM.find k s with Not_found -> 
    failure "ERROR: sol_read : unknown kvar %s \n" (Sy.to_string k)

let sol_update s k qs' =
  let qs = sol_read s k in
  (not (Misc.same_length qs qs'), SM.add k qs' s)

(* API *)
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

(* API *)
let apply_substs xes p = 
  List.fold_left (fun p' (x,e) -> P.subst p' x e) p xes

(* API *)
let refineatom_preds s   = function
  | Conc p       -> [p]
  | Kvar (xes,k) -> List.map (apply_substs xes) (sol_read s k)

(* API *)
let refinement_preds s (_,_,ras) =
  Misc.flap (refineatom_preds s) ras

(* API *)
let environment_preds s env =
  SM.fold
    (fun x ((vv, t, ras) as r) ps -> 
      let vps = refinement_preds s r in
      let xps = List.map (fun p -> P.subst p vv (A.eVar x)) vps in
      xps ++ ps)
    env [] 

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let print_sub ppf (x,e) = 
  Format.fprintf ppf "[%a:=%a]" Sy.print x E.print e

let print_refineatom ppf = function
  | Conc p        -> Format.fprintf ppf "%a" P.print p
  | Kvar (xes, k) -> Format.fprintf ppf "%a[%a]" Sy.print k 
                       (Misc.pprint_many false "" print_sub) xes

let print_refinement ppf (v, t, ras) =
  Format.fprintf ppf "@[{%a : %a | %a}@]" 
    Sy.print v
    Ast.Sort.print t
    (Misc.pprint_many false " /\ " print_refineatom) ras  

let print_binding ppf (x, r) = 
  Format.fprintf ppf "@[%a => %a@],@;<0 2>" 
    Sy.print x 
    print_refinement r 

let print_env so ppf env = 
  match so with
  | None   -> SM.iter (fun x y -> print_binding ppf (x, y)) env 
  | Some s -> environment_preds s env |> 
              Format.fprintf ppf "%a" (Misc.pprint_many false "&&" P.print) 

let pprint_io ppf = function
  | Some id -> Format.fprintf ppf "(%d)" id
  | None    -> Format.fprintf ppf "()"

(* API *)
let print so ppf (env,g,r1,r2,io) =
  Format.fprintf ppf 
    "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
    pprint_io io 
    (print_env so) env 
    P.print g
    print_refinement r1
    print_refinement r2

(* API *) 
let to_string c = Misc.fsprintf (print None) c

(* API *)
let print_soln ppf sm =
  Format.fprintf ppf "Solution: \n";
  SM.iter 
    (fun x ps -> 
       Format.fprintf ppf "%a := %a \n" 
       Ast.Symbol.print x (Misc.pprint_many false "," P.print) ps)
    sm

