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
let print     = failwith "TBD" 

let refinement_kvars r =
  List.fold_left 
    (fun ks a -> match a with Kvar (_,k) -> k::ks | _ -> ks) 
    [] r 

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

(**************************************************************)
(********************** Pretty Printing ***********************)
(**************************************************************)
(*
   let pprint_local_binding f ppf = function
  | (Path.Pident _ as k, v) -> 
      fprintf ppf "@[%s@ =>@ %a@],@;<0 2>" 
      (Path.unique_name k) f v
  | _ -> ()

let pprint_fenv ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint f) (F.prune_background env); fprintf ppf "==="

let pprint_fenv_shp ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint (F.shape f)) (F.prune_background env); fprintf ppf "==="

let pprint_raw_fenv shp ppf env =
  Le.iter (fun p f -> fprintf ppf "@[%s@ ::@ %a@]@." (C.path_name p) F.pprint (if shp then F.shape f else f)) env; fprintf ppf "==="

let pprint_fenv_pred so ppf env =
  Le.iter (fun x t -> pprint_local_binding F.pprint ppf (x, t)) env

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
*)
let pprint_ref so ppf (renv,g,r1,sr2,io) =
  failwith "TBD"
  (*
  let renv = F.prune_background renv in
  fprintf ppf "@[%a@ Env:@ @[%a@];@;<1 2>Guard:@ %a@\n|-@;<1 2>%a@;<1 2><:@;<1 2>%a@]"
  pprint_io io (pprint_renv_pred F.pprint_refinement so) renv 
  P.pprint (guard_predicate () g) 
  F.pprint_refinement r1 F.pprint_refinement (F.ref_of_simple sr2)
  *)


