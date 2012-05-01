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

module P  = Pretty
module M  = FixMisc
module Sl = Sloc
module Ct = Ctypes
module FC = FixConstraint
module SlS  = Sl.Subst
module SlSS = Sl.SlocSlocSet
module CtI  = Ct.I
module CtIS = CtI.Store
module CtIF = CtI.Field
module CtIC = CtI.CType

open M.Ops

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

module VR  = Ct.VarRefinement
module RVT = Ct.RefVarTypes
module RVS = Ct.Make(RVT)

type intrs       = Sl.t list

type 'a def      = { loc_params :  Sl.t list 
                   ; ref_params :    'a list 
                   ; unfolds    : intrs list
                   ; rhs        : 'a Ctypes.prestore
                   }

type ref_def  = FC.reft def
type var_def  = VR.t def

module HfMap = M.StringMap
type   env   = var_def HfMap.t

let flocs_of def = def.loc_params
let frefs_of def = def.ref_params
let unfs_of  def = def.unfolds
let rhs_of   def = def.rhs

(*let wf app def =
  wd_def def and wf

let wf_def { loc_params = ls
           ; ref_params = rs
           ; unfolds    = ins
           ; rhs        = h } =
  List.length ls = List.length ins and
  apply ("", ls, rs) ins def = h*)

let rec ind_of_rv = function
  | Ct.Int (d, (_, i))  -> Ct.Int(d, i)
  | Ct.Ref (s, (_, i))  -> Ct.Ref(s, i)
  | Ct.FRef (f, (_, i)) -> Ct.FRef((RVS.CFun.map ind_of_rv f), i)
  | Ct.ARef -> Ct.ARef
  | Ct.Any  -> Ct.Any

let apply_hf_shape (_, sls, _) ins def =
  let ls, is = def |> flocs_of |> M.flip List.combine sls,
               def |> unfs_of  |> M.flip M.flapcombine ins in
  let hp     = def |> rhs_of   |> RVS.Store.map ind_of_rv
                               |> CtIS.subs (ls @ is) in
  let deps   = List.fold_left (M.flip SlSS.add) SlSS.empty ls in
    deps, hp

let apply_in_env ((f, _, _) as app) ins env =
  HfMap.find f env |>
  apply_hf_shape app ins 



(*let invert_env l hf ins env =
  SlSS.find hf env |> invert_hf_from_hp l hf ins*)

(*let invert_hf_from_hp l hf ins def h =
  let fls, frs, fils, rhs =
    def.loc_params, def.ref_params, def.unfolds, def.rhs in
  let loc_map = ld_looks_like l h rhs in
  (hf, looks_like rhs h, [])*)
   
  (* h(l) <: h'(l) *)

let heap_infer_subs h1 h2 =
  let subs = ref [] in 
  let upd = function
    | (Some x, Some y) -> subs := (x,y) :: !subs
    | _ -> () in
  let f t1 t2 =
    let _ = assert (CtIC.same_shape t1 t2) in
    M.map_pair CtIC.sloc (t1, t2) |> upd in
  CtIS.iter2 f h1 h2; !subs


let binding_of l =
  List.find (function (_, k :: _, _) -> k = l | _ -> false)

let binds l hfs =
  try
    binding_of l hfs; true
  with Not_found -> false

(*let ins l (_, _, hfs) ls env =
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
