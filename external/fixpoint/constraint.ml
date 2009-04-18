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
