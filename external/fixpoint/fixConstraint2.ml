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
module F  = Format
module A  = Ast
module E  = A.Expression
module P  = A.Predicate
module Sy = A.Symbol
module SM = Sy.SMap
module BS = BNstats

open Misc.Ops

type tag  = int
type subs = (Sy.t * A.expr) list                         (* [x,e] *)
type refa = Conc of A.pred | Kvar of subs * Sy.t
type reft = Sy.t * A.Sort.t * (refa list)                (* { VV: t | [ra] } *)
type envt = reft SM.t
type soln = A.pred list SM.t
type t    = (envt * envt) * A.pred * reft * reft * (tag option)
type wf   = envt * reft * (tag option)

type deft = Srt of Ast.Sort.t 
          | Axm of Ast.pred 
          | Cst of t
          | Wfc of wf
          | Sol of Ast.Symbol.t * Ast.pred list
          | Qul of Ast.Qualifier.t

(*************************************************************)
(************************** Misc.  ***************************)
(*************************************************************)

let is_simple_refatom = function 
  | Kvar ([], _) -> true
  | _            -> false

let kvars_of_reft (_, _, rs) =
  Misc.map_partial 
    (function Kvar (subs,k) -> Some (subs,k) 
            | _             -> None) 
    rs

let env_of_bindings xrs =
  List.fold_left begin
    fun env (x, r) -> 
      if not (SM.mem x env) then SM.add x r env else
        ((if not (r = SM.find x env) then 
          Printf.printf "WARNING: env_of_bindings : duplicate %s \n" (Sy.to_string x)); 
         env) 
       
  end SM.empty xrs

let bindings_of_env env = 
  SM.fold (fun x y bs -> (x,y)::bs) env []

(* API *)
let is_simple (_,_,(_,_,ra1s),(_,_,ra2s),_) = 
  List.for_all is_simple_refatom ra1s 
  && List.for_all is_simple_refatom ra2s 
  && (not !Constants.no_simple) 
  && (not !Constants.verify_simple)

(* API *)
let kvars_of_t ((_,env), _, lhs, rhs, _) =
  [lhs; rhs] 
  |> SM.fold (fun _ r acc -> r :: acc) env
  |> Misc.flap kvars_of_reft 

(*************************************************************)
(******************** Solution Management ********************)
(*************************************************************)

(* API *)
let sol_cleanup s = 
  SM.map Misc.sort_and_compact s

(* API *)
let sol_query s k =
  try SM.find k s with Not_found -> []

(* API *)
let sol_read s k = 
  try SM.find k s with Not_found -> 
    failure "ERROR: sol_read : unknown kvar %s \n" (Sy.to_string k)

(* INV: qs' \subseteq qs *)
let sol_update s k qs' =
  let qs = sol_read s k in
  (not (Misc.same_length qs qs'), SM.add k qs' s)

(* API *)
let sol_add s k qs' = 
  let qs   = sol_query s k in
  let qs'' = qs' ++ qs in
  (not (Misc.same_length qs qs''), SM.add k qs'' s)

let group_sol_change addf s0 ks kqs = 
  let t  = Hashtbl.create 17 in
  let _  = List.iter (fun (k, q) -> Hashtbl.add t k q) kqs in
  List.fold_left 
    (fun (b, s) k -> 
      let qs       = Hashtbl.find_all t k in 
      let (b', s') = if addf then sol_add s k qs else sol_update s k qs in
      (b || b', s'))
    (false, s0) ks

(* API *)
let group_sol_update = group_sol_change false
let group_sol_add    = group_sol_change true

(*************************************************************)
(*********************** Logic Embedding *********************)
(*************************************************************)

let non_trivial env = 
  SM.fold begin fun x r sm -> match thd3 r with 
        | [] -> sm 
        | _::_ -> SM.add x r sm
  end env SM.empty

(* API *)
let apply_substs xes p = 
  List.fold_left (fun p' (x,e) -> P.subst p' x e) p xes

(* API *)
let preds_of_refa s   = function
  | Conc p       -> [p]
  | Kvar (xes,k) -> List.map (apply_substs xes) (sol_read s k)

(* API *)
let preds_of_reft s (_,_,ras) =
  Misc.flap (preds_of_refa s) ras

let preds_of_envt s env =
  SM.fold
    (fun x ((vv, t, ras) as r) ps -> 
      let vps = preds_of_reft s r in
      let xps = List.map (fun p -> P.subst p vv (A.eVar x)) vps in
      xps ++ ps)
    env [] 

(* API *)
let preds_of_lhs s ((_,env), gp, r1, _, _) =
  let envps = preds_of_envt s env in
  let r1ps  = preds_of_reft s r1 in
  gp :: (envps ++ r1ps) 

(* API *)
let vars_of_t s ((_, _, _, r2, _) as c) =
  (preds_of_reft s r2) ++ (preds_of_lhs s c)
  |> Misc.flap P.support
  

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)

let print_sub ppf (x,e) = 
  F.fprintf ppf "[%a:=%a]" Sy.print x E.print e

let print_refineatom ppf = function
  | Conc p        -> F.fprintf ppf "%a" P.print p
  | Kvar (xes, k) -> F.fprintf ppf "%a%a" Sy.print k 
                       (Misc.pprint_many false "" print_sub) xes

let print_ras so ppf ras = 
  match so with 
  | None   -> F.fprintf ppf "%a" (Misc.pprint_many false ";" print_refineatom) ras
  | Some s -> ras |> Misc.flap (preds_of_refa s) |> A.pAnd
                  |> F.fprintf ppf "%a" P.print 

let print_reft so ppf (v, t, ras) =
  F.fprintf ppf "@[{%a : %a | [%a]}@]" 
    Sy.print v
    Ast.Sort.print t
    (print_ras so) ras

let print_binding so ppf (x, r) = 
  F.fprintf ppf "@[%a:%a@]" 
    Sy.print x 
    (print_reft so) r 

let print_env so ppf env = 
  bindings_of_env env 
  |> F.fprintf ppf "@[%a@]" (Misc.pprint_many_box ";" (print_binding so))

let pprint_tag ppf = function
  | Some id     -> F.fprintf ppf "tag %d" id
  | None        -> F.fprintf ppf ""

(* API *)
let print_wf so ppf (env, r, io) = 
  F.fprintf ppf "wf: env @[[%a]@] @\n reft %a @\n %a @\n"
    (print_env so) env
    (print_reft so) r
    pprint_tag io

(* API *)
let print_t so ppf ((env,_),g,r1,r2,io) =
  F.fprintf ppf 
  "constraint:@.  env  @[[%a]@] @\n grd @[%a@] @\n lhs @[%a@] @\n rhs @[%a@] @\n %a @\n"
    (print_env so) env 
    P.print g
    (print_reft so) r1
    (print_reft so) r2
    pprint_tag io

(* API *) 
let to_string c = Misc.fsprintf (print_t None) c

let refa_to_string = function
  | Conc p -> P.to_string p
  | Kvar (subs, sym) ->
      Printf.sprintf "%s%s" (Sy.to_string sym)
	(List.map
	   (fun (s, e) -> 
	      Printf.sprintf "[%s/%s]" 
		(E.to_string e) (Sy.to_string s)
	   ) subs |> String.concat "")

let reft_to_string (vv, sort, ras) =
  Printf.sprintf "{%s:%s | [%s]}"
    (Sy.to_string vv)
    (Ast.Sort.to_string sort)
    (List.map refa_to_string ras |> String.concat ", ")

let binding_to_string (vv, reft) =
  Printf.sprintf "%s:%s" (Sy.to_string vv) (reft_to_string reft)

(* API *)
let print_soln ppf sm =
  SM.iter 
    (fun k ps -> 
      F.fprintf ppf "solution: @[%a := [%a]@] \n"  
        Sy.print k (Misc.pprint_many false ";" P.print) ps)
    sm

(***************************************************************)
(*********************** Getter/Setter *************************)
(***************************************************************)

let theta_ra subs = function
  | Conc p          -> Conc (apply_substs subs p)
  | Kvar (subs', k) -> Kvar (subs ++ subs', k)

(* API *)
let make_reft     = fun v so ras -> (v, so, ras)
let vv_of_reft    = fst3
let sort_of_reft  = snd3
let ras_of_reft   = thd3
let shape_of_reft = fun (v, so, _) -> (v, so, [])
let theta         = fun subs (v, so, ras) -> (v, so, Misc.map (theta_ra subs) ras)

(* API *)
let make_t      = fun env p r1 r2 io -> ((env, non_trivial env ), p, r1, r2, io)
let env_of_t    = fun ((env,_),_,_,_,_) -> env
let grd_of_t    = fun (_,grd,_,_,_) -> grd 
let lhs_of_t    = fun (_,_,lhs,_,_) -> lhs 
let rhs_of_t    = fun (_,_,_,rhs,_) -> rhs
let id_of_t     = function (_,_,_,_,Some i) -> i | _ -> assertf "C.id_of_t"

(* API *)
let make_wf     = fun env r io -> (env, r, io)
let env_of_wf   = fst3
let reft_of_wf  = snd3
let id_of_wf    = function (_,_,Some i) -> i | _ -> assertf "C.id_of_wf"

(***************************************************************)
(********************** Input Validation ***********************)
(***************************************************************)

(* 1. check tags are distinct, return max tag *)
let phase1 cs = 
  let tags = Misc.map_partial (fun (_,_,_,_,x) -> x) cs in
  let _    = asserts (Misc.distinct tags) "Invalid Constraints 1" in
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
  List.iter begin fun ((env,_),_,(vv1,t1,_),(vv2,t2,_),_) ->
    asserts (vv1 = vv2 && t1 = t2) "Invalid Constraints 3a"; 
    SM.iter begin fun x (_,t,_) ->
      try asserts (t = (Hashtbl.find memo x)) 
            "Invalid Constraints 3b" 
      with Not_found -> 
        Hashtbl.replace memo x t
    end env
  end cs;
  cs

let validate cs = 
  phase1 cs |> phase2 cs |> phase3

let validate cs = 
  BS.time "validate shapes" validate cs
