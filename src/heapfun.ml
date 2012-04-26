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

module M  = FixMisc
module Sl = Sloc
module Ct = Ctypes
module FC = FixConstraint
module SlS  = Sl.Subst
module SlSS = Sl.SlocSlocSet
module CtR  = Ct.RefCTypes
module CtS  = CtR.Store
module CtD  = CtS.Data

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

type refVar      = string

type intrs       = Sl.t list

type 'a def      = { loc_params :  Sl.t list 
                   ; ref_params :    'a list 
                   ; unfolds    : intrs list
                   ; rhs        : 'a Ctypes.prestore
                   }

type ref_def  = FC.reft def
type var_def  = refVar  def

module HfMap = M.StringMap
type   env   = var_def HfMap.t

(*let formal_locs_of_def def = def.loc_params 
let formal_vars_of_def def = def.ref_params
let unfolds_of_def     def = def.unfolds

let wf_def { loc_params = ls
           ; ref_params = rs
           ; unfolds    = is
           ; rhs        = h } =
  List.length ls              = List.length is and
  (apply ("", ls, rs) is def) = h

let apply_in_env ((f, _, _) as app) ins env =
  HfMap.get f env |>
  apply app ins 

let apply_rs_to_hp rs rhs =
  CtS.map (List.assoc rs |> CtC.map) rhs

let apply_hf ((f, sls, rls) as appl) ins def =
  let rs, ls = M.combine_prefix def.loc_params sls, List.combine def.ref_params rls in
  List.fold_left (M.flip SlSS.add) SlSS.empty ls,
  (SlS.apply def.rhs sls |> apply_rs_to_hp rs)

(*let invert_env l hf ins env =
  SlSS.find hf env |> invert_hf_from_hp l hf ins

let invert_hf_from_hp l hf ins def h =
  let fls, frs, fils, rhs =
    def.loc_params, def.ref_params, def.unfolds, def.rhs in
  let loc_map = ld_looks_like l h rhs in
  (hf, looks_like rhs h, [])
   
  (* h(l) <: h'(l) *)
let rec data_looks_like l (ds, _, _) (ds, _, _) = 
  (fun x -> CtD. x (CtD.find l ds) (CtD.find l ds'))
  <| (function (Ref(l1, i1, r1), Ref(l2, _, _)) -> Ref(l1, i1, r1)
  *)

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
    | Some hf -> apply_in_env hf ls env |> H.append hfs 
    | None    -> (SlSS.empty, hfs)
  
(*let gen l (ds, _, hfs) ls env =
  let _  = assert CtD.mem l ds in
  CtD.find l ds
  *)

(*
let def_of_list a =

let d_ref_def =
let d_var_def =*)

*)
