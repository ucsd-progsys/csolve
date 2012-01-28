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
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONAst.Symbol.
 *
 *)

(* This module implements the IMP language and translation from fixpoint constraints *)


module F  = Format
module H  = Hashtbl
module A  = Ast
module E  = A.Expression
module P  = A.Predicate
module Sy = A.Symbol
module SM = Sy.SMap
module SS = Sy.SSet
module C  = FixConstraint
module Cg = FixConfig
(*module BS = BNstats*)

open Imp
open Misc.Ops


(*************************************************************************)
(************* Datatypes for SMTLIB Representation ***********************)
(*************************************************************************)

type rpred  
  = A.pred                      (* Bexp (App (k, es)) *) 

type vdef
  = Sy.t * So.t                 (* variable, sort *)

type kdef
  = Sy.t * (vdef list)

type cstr
  = A.pred * rpred option

type smtlib 
  = { vars  : vdef list
    ; kvars : kdef list
    ; cstrs : cstr list 
  }

type kmap 
  = kdef SM.t

(*************************************************************************)
(************* Rendering SMTLIB to String ********************************)
(*************************************************************************)

let print_vdef ppf (x, t) = 
  failwith "TODO"

let print_kdef ppf (kf, xts) = 
  failwith "TODO"

let print_cstr ppf = function 
  | (p, None)   -> failwith "TODO"
  | (p, Some q) -> failwith "TODO"

let print ppf smt = 
  Format.fprintf ppf 
    "(benchmark unknown
     :status unsat
     :logic AUFLIA
     %a\n%a\n%a\n
     )"
    (Misc.pprint_many true "\n" print_vdef) smt.vars
    (Misc.pprint_many true "\n" print_kdef) smt.kvars
    (Misc.pprint_many true "\n" print_cstr) smt.cstrs

(*************************************************************************)
(************* Helpers for extracting var-sort bindings ******************) 
(*************************************************************************)

let sort_compat x t t' =
  Ast.Sort.compat t t'
  >> (fun b -> if not b then 
               Printf.printf "WARNING: k-sort incompatible for %s" 
               (Sy.to_string x))

let vdefs_of_env env r = 
  env |> C.bindings_of_env
      |> (++) [(C.vv_of_reft r, r)]
      |> List.map (Misc.app_snd C.sort_of_reft)
      |> Misc.fsort fst

(*************************************************************************)
(************* Build VMap : gather all vars/sorts for regular vars *******) 
(*************************************************************************)

let update_vmap vm (x, t) =
  Misc.maybe_iter begin fun t' ->
    asserts (sort_compat x t t') "ERROR: v-sort incompatible %s" (Sy.to_string x)
  end (SM.maybe_find x vm);
  SM.add x t vm

let add_var_to_vmap vm c : SM.t =
  let vvl = C.vv_of_reft (C.lhs_of_t c) in
  let vvr = C.vv_of_reft (C.rhs_of_t c) in
  let _   = asserts (vvl = vvr) "Different VVs in Constr: %d" (C.id_of_t c) in
  vdefs_of_env (C.env_of_t c) (C.lhs_of_t c)
  |> List.fold_left update_vmap vm 
  
(*************************************************************************)
(************ Build KMap: gather scopes for each kvar from wfs** *********)
(*************************************************************************)

let check_no_subs wid suks = 
  asserts 
    (List.for_all (fst <+> Su.is_empty) suks) 
    "NonTriv Subst wid=%d" wid

let join vds vds' = 
  let xm  = SM.of_list vds  in
  List.filter begin fun (x, t) ->
    match SM.maybe_find x xm with
    | None    -> false
    | Some t' -> sort_compat x t t'  
  end vds'       

let update_kmap vdefs km k : kmap =
  match SM.maybe_find k km with
  | None        -> SM.add k vdefs km
  | Some vdefs' -> SM.add k (join vdefs vdefs') km

let add_wf_to_kmap km wf =
  let vdefs = vdefs_of_env (C.env_of_wf wf) (C.reft_of_wf wf) in
  C.reft_of_wf wf
  |> C.kvars_of_reft  
  >> check_no_subs (C.id_of_wf wf) 
  |> List.map snd
  |> List.fold_left (update_kmap vdefs) km

let make_kmap defs : kmap = 
  defs 
  |> Misc.map_partial (function Cg.Wfc x -> Some x | _ -> None)
  |> List.fold_left add_wf_to_kmap SM.empty

(*************************************************************************)
(************* Translating using the KMap ********************************)
(*************************************************************************)

let pred_of_kdef (kf, xts) =
  A.eApp (kf, List.map (fst <+> A.eVar) xts)  

let soln_of_kmap km k =
  pred_of_kdef <| SM.safeFind k km "soln_of_kmap"

let tx_constraint s c =
  let lps     = C.preds_of_lhs s c in
  let v,t,ras = C.rhs_of_t c       in
  foreach ras begin function 
    | Conc p -> (A.pAnd ((A.pNot p) :: lps), None)
    | ra     -> (A.pAnd lps, Some (C.preds_of_refa s ta))
  end

let tx_defs (defs : FixConfig.t list) : smtlib = 
  let km  = defs |> make_kmap in
  let s   = soln_of_kmap km   in 
  let cs  = defs |> Misc.map_partial (function Cg.Cst x -> Some x | _ -> None) 
                 |> Misc.map canonize_vv in
  { vars  = SS.elements <| List.fold_left add_vars SS.empty cs
  ; kvars = SM.range km
  ; cstrs = Misc.map (tx_constraint s) cs 
  }


(*************************************************************************)
(************* API *******************************************************)
(*************************************************************************)

let render ppf defs =
  defs |> defs_to_smt 
       |> F.fprintf ppf "%a" print
