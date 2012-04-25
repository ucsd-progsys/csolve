(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
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

module E  = ErrorMsg
module M  = FixMisc
module Sl = Sloc
module Ct = Ctypes
module FC = FixConstraint
module SlS  = Sl.Subs
module SlSS = Sl.SlocSlocSet
module CtS  = Ct.Reft.Store
module CtD  = Ct.Reft.Store.Data

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

type refVar      = string

type intr        = Sloc.t list

type 'a def      = { loc_params : Sloc.t list 
                   ; ref_params :     'a list 
                   ; unfolds    :   intr list
                   ; rhs        : 'a Ctypes.prestore
                   }

type ref_def  = FixConstraint.reft def
type var_def  = refVar def

module HfMap = FixMisc.StringMap
type   env   = var_def HfMap.t

let intr_is_conc  (_, l2) = Sloc.is_concrete l2
let intr_is_abs   (_, l2) = Sloc.is_abstract l2
let intr_has_conc (_, ls) = List.exists Sloc.is_abs  ls
let intr_has_abs  (_, ls) = List.exists Sloc.is_conc ls
let intr_all_conc (_, ls) = List.for_all Sloc.is_abs ls
let intr_all_abs  (_, ls) = List.for_all Sloc.is_abs ls

(*let test_env    = HfMap.add "list" def_of_list HfMap.empty*)

let wf_def { loc_params = ls
           ; ref_params = rs
           ; unfolds    = is
           ; rhs        = h } =
  List.length ls              = List.length is and
  (apply ("", ls, rs) is def) = h

let apply_on_env ((f, _, _) as app) ins env =
  HfMap.get f env |>
  apply app ins 

let apply_rs_to_hp rs rhs =
  CtS.map (List.assoc rs |> CtC.map) rhs

let apply_hf ((f, sls, rls) as appl) ins def =
  let rs, ls = M.combine_prefix def.loc_params sls, List.combine def.ref_params rls in
  List.fold_left (M.flip SlSS.add) SlSS.empty ls,
  (SlS.apply def.rhs sls |> apply_rs_to_hp rs)

let invert_env l hf ins env =
  SlSS.find hf env |> invert_hf_from_hp l hf ins

let invert_hf_from_hp l hf ins def =
  let fls, frs, fils, rhs =
    def.loc_params, def.ref_params, def.unfolds, def.rhs in
   
let rec looks_like h h' = 

let binding_of l hfs =
  let hf =
    M.only_one "binding_of: too many binds of location"
      <| M.flip List.filter hfs
      <| (function x :: _ -> x = l | _ -> false) in
  let hfs = function None -> hfs | Some x -> List.remove x hfs in
    (hf, hfs hf)

let ins l (_, _, hfs) ls env =
  let (hf, hfs) = binding_of l hfs in
  match hf with
    | Some hf -> apply_on_env hf ls env |> H.append hfs 
    | None    -> (SlSS.empty, hfs)
  
let gen l (ds, _, hfs) ls env =
  let _  = assert CtD.mem l ds in
  CtD.find l ds
(*
let def_of_list a =

let d_ref_def =
let d_var_def =*)


