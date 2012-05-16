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
module I  = Index
module Sl = Sloc
module Ct = Ctypes
module FC = FixConstraint
module CM = CilMisc
module SlS  = Sl.Subst
module SlSS = Sl.SlocSlocSet
module CtI  = Ct.I
module CtIS = CtI.Store
module CtIF = CtI.Field
module CtIC = CtI.CType
module CtIL = CtI.LDesc

open M.Ops

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

module VR  = Ct.VarRefinement
module RVT = Ct.RefVarTypes
module RVS = Ct.Make(RVT)

type intrs       = Sl.t list

type 'a def      = { loc_params : Sl.t list 
                   ; ref_params : 'a list 
                   ; unfolds    : intrs list
                   ; rhs        : 'a Ctypes.prestore
                   }

type ref_def  = FC.reft def
type var_def  = VR.t def
type ind_def  = CtI.T.refinement def

module HfMap = M.StringMap
type   env   = var_def HfMap.t

let flocs_of def = def.loc_params
let frefs_of def = def.ref_params
let unfs_of  def = def.unfolds
let rhs_of   def = def.rhs


let def_of_intlist =
  let nm = "intlist" in
  let l  = Sl.fresh_abstract (CM.srcinfo_of_string "intlist_def") in
  let (lj, l') = (Sl.copy_concrete l, Sl.copy_fresh l) in
  let ld = CtIL.create {Ct.stype = None; Ct.any = false} 
    [(I.of_int 0, CtIF.create Ct.Final    Ct.dummy_fieldinfo Ct.int_ctype);
     (I.of_int 4, CtIF.create Ct.Nonfinal Ct.dummy_fieldinfo (Ct.ptr_ctype_of l'))] in
  let ap = (nm, [l'], []) in
  { loc_params = [l]
  ; ref_params = []
  ; unfolds    = [[lj; l']]
  ; rhs        = CtIS.add CtIS.empty lj ld |> M.flip CtIS.add_app ap
  }

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

let apply_hf_in_env ((f, _, _) as app) ins env =
  HfMap.find f env |> apply_hf_shape app ins

  (* remove dependencies *)
let fold_hf_on_hp ls ins h hf env =
  let binds   = match ls with
    | l :: _ -> l :: List.flatten ins 
    | _      -> List.flatten ins in
  let (h, h') = CtIS.partition (fun l -> List.mem l binds) h in
  let  appl   = (hf, ls, []) in
  let happl   = apply_hf_in_env appl ins env in
  let _       = if (snd happl != h) then
    assertf "fold_hf_on_hp: can't fold on heap" in 
    CtIS.add_app h' appl

let binding_of l =
  List.find (function (_, k :: _, _) -> k = l | _ -> false)

let arg_of l =
  List.partition (fun (_, ls, _) -> List.mem l ls)

let binds l hfs =
  try
    binding_of l hfs; true
  with Not_found -> false

let ins l sto deps ls ins env =
  let binding_fun  = CtIS.hfuns sto |> binding_of l |> fst3 in
  (*let ins          = binding_fun |> Misc.flap HfMap.find env |>
                     unfs_of |> Sl.copy_fresh in*)
  let (deps', unfolded_sto) =
    apply_hf_in_env (binding_fun, ls, []) ins env in
  SlSS.union deps deps', CtIS.upd sto unfolded_sto 

let gen l hf sto deps ls ins env =
  let deps = M.combine_prefix ls ins |>
             List.fold_left (M.flip SlSS.remove) deps in
  deps, fold_hf_on_hp ls ins sto hf env 

let shape_in_env hf ls env =
  let ins = HfMap.find hf env |> unfs_of |> M.combine_replace ls in
  apply_hf_in_env (hf, ls, []) ins env
