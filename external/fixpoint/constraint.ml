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

module S  = Ast.Symbol
module E  = Ast.Expression
module P  = Ast.Predicate
module SM = S.SMap

type tag  = int
type subs = (S.t * E.t) list                    (* [x,e] *)
type refa = Conc of P.t | Kvar of subs * S.t
type reft = S.t * (refa list)                   (* VV, [ra] *)
type envt = (Sort.t * reft) SM.t
type soln = P.t list SM.t
type t    = envt * P.t * reft * reft * (tag option) 


(*************************************************************)
(************************** Random ***************************)
(*************************************************************)

let is_simple_refatom = function 
  | C.Kvar ([], _) -> true
  | _ -> false

(* API *)
let is_simple (_,_,(_,ra1s),(_,ra2s),_) = 
  List.for_all is_simple_refatom ra1s &&
  List.for_all is_simple_refatom ra2s &&
  not (!Co.no_simple || !Co.verify_simple)

(*************************************************************)
(******************** Solution Management ********************)
(*************************************************************)

(* API *)
let sol_read s k = 
  try SM.find s k with Not_found -> 
    failure "ERROR: sol_read : unknown kvar %s \n" s

let sol_update s k qs' =
  let qs = sol_read s k in
  (not (Misc.same_length qs qs'), SM.replace s k qs')

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

(* API *)
let print so ppf (env,g,r1,r2,io) =
  Format.fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
    pprint_io io 
    (print_env so) env 
    P.print g
    print_refinement r1
    print_refinement r2

(* API *) 
let to_string c = Format.sprintf "%a" (print None) c
