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

module C  = Cil
module P  = Pretty
module M  = FixMisc
module I  = Index
module SM = M.StringMap
module Sl = Sloc
module Ct = Ctypes
module FC = FixConstraint
module CM = CilMisc
module SlS  = Sl.Subst
module SlSS = Sl.SlocSlocSet
module CtR  = Ct.RefCTypes
module CtRS = CtR.Store
module CtI  = Ct.I
module CtIS = CtI.Store
module CtIF = CtI.Field
module CtIC = CtI.CType
module CtIL = CtI.LDesc
module CtISp = CtI.Spec
module CtICF = CtI.CFun

open M.Ops

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

module VR  = Ct.VarRefinement
module RV  = Ct.RV
module RVS = RV.Store
module RVF = RV.Field
module RVC = RV.CType
module RVL = RV.LDesc

type intrs       = Sl.t list

type def      = { loc_params : Sl.t list 
                ; ref_params : VR.t list 
                ; unfolds    : intrs list
                ; rhs        : VR.t Ctypes.prestore
                }

module HfMap = M.StringMap
type   env   = def HfMap.t

let flocs_of def = def.loc_params
let frefs_of def = def.ref_params
let unfs_of  def = def.unfolds
let rhs_of   def = def.rhs

let fresh_unfs_of_hf cl hf env = 
  HfMap.find hf env
  |> unfs_of
  |> M.map_nested Sl.copy_fresh
  |> M.map_fst (M.map_fst (fun _ -> cl))

let def_of_intlist =
  let nrf = VR.top in
  let nr0 = VR.of_const CM.cZero in
  let nm = "intlist" in
  let l  = Sl.fresh_abstract (CM.srcinfo_of_string "intlist_def") in
  let (lj, l') = (Sl.copy_concrete l, Sl.copy_fresh l) in
  let ld = RVL.create {Ct.stype = None; Ct.any = false} 
    [(I.of_int 0, RVF.create Ct.Final    Ct.dummy_fieldinfo (Ct.Int(4, nrf)));
     (I.of_int 4, RVF.create Ct.Nonfinal Ct.dummy_fieldinfo (Ct.Ref(l', nr0)))] in
  let ap = (nm, [l'], []) in
  { loc_params = [l]
  ; ref_params = []
  ; unfolds    = [[lj; l']]
  ; rhs        = RVS.add RVS.empty lj ld |> M.flip RVS.add_app ap
  }

let test_env =
  HfMap.add "intlist" def_of_intlist HfMap.empty

(*let wf app def =
  wd_def def and wf

let wf_def { loc_params = ls
           ; ref_params = rs
           ; unfolds    = ins
           ; rhs        = h } =
  List.length ls = List.length ins and
  apply ("", ls, rs) ins def = h*)

let rec top_out_unks = function
  | Ct.Int (d, (_, i))  -> Ct.Int  (d, (i, Ct.reft_of_top))
  | Ct.Ref (s, (_, i))  -> Ct.Ref  (s, (i, Ct.reft_of_top))
  | Ct.FRef (f, (_, i)) ->
      Ct.FRef ((RV.CFun.map top_out_unks f), (i, Ct.reft_of_top))
  | Ct.ARef             -> Ct.ARef
  | Ct.Any              -> Ct.Any

let rec strip_refts = function
  | Ct.Int (d, (i, _))  -> Ct.Int  (d, i)
  | Ct.Ref (s, (i, _))  -> Ct.Ref  (s, i)
  | Ct.FRef (f, (i, _)) ->
      Ct.FRef ((CtR.CFun.map strip_refts f), i)
  | Ct.ARef             -> Ct.ARef
  | Ct.Any              -> Ct.Any

let isto_of_rsto =
  CtRS.map strip_refts

let apply_hf_shape (_, sls, _) ins def =
  let ls, is = def |> flocs_of |> M.flip List.combine sls,
               def |> unfs_of  |> M.flip M.flapcombine ins in
  let hp     = def |> rhs_of   |> RVS.map (top_out_unks)
                               |> CtRS.subs (ls @ is) in
  let deps   = List.fold_left (M.flip SlSS.add) SlSS.empty ls in
    deps, hp

let apply_hf_in_env ((hf, _, _) as app) ins env =
     HfMap.find hf env
  |> apply_hf_shape app ins

  (* remove dependencies *)
(* in the current implementation we're going to assume
 * that the only argument that can be bound to an ldesc
 * in the expansion of an hfun is the first argument.
 * BUT, any introduced location could be bound to an
 * ldesc. *)
(*let fold_hf_on_sto ((hf, ls, _) as app) ins sto env =
  let bound_locs     = List.hd ls :: List.flatten ins
                    |> M.sort_and_compact in
  let (exp, sto')    = CtIS.partition (M.flip List.mem bound_locs) sto in
  let desired_exp    = apply_hf_in_env app ins env
                    |> snd
                    |> isto_of_rsto in
    if (CtIS.eq exp desired_exp) then
      CtIS.add_app sto' app
    else
      sto
      *)
let fold_hf_on_sto a b c d = assert false
  (* MK: this should be heap subtyping *)

let fold_hf_shapes_on_sto ((hf, ls, _) as app) ins sto env =
  let bound_locs     = List.hd ls :: List.flatten ins
                    |> M.sort_and_compact in
  let (exp, sto')    = CtIS.partition (M.flip List.mem bound_locs) sto in
  let desired_exp    = apply_hf_in_env app ins env
                    |> snd
                    |> isto_of_rsto in
    if (CtIS.eq exp desired_exp) then
      CtIS.add_app sto' app
    else
      sto
 
let ins_shp l ls ins deps sto env =
  let (hf, ls', _) =
       CtIS.hfuns sto
    |> Ct.hf_appl_binding_of l
    |> M.maybe in
  apply_hf_in_env (hf, ls, []) ins env
  |> M.app_snd isto_of_rsto
  |> M.app_fst (SlSS.union deps)
  |> M.app_snd (CtIS.remove sto l |> CtIS.upd)


let gen_shp ((hf, ls, _) as app) ins deps sto env =
  let deps = M.combine_prefix ls ins |>
             List.fold_left begin fun deps ll ->
               try SlSS.remove ll deps with Not_found -> deps end deps in
  let sto  = fold_hf_shapes_on_sto (hf, ls, []) ins sto env in
  deps, sto

let unfs_of_ident ls hf env =
     HfMap.find hf env
  |> unfs_of
  |> M.combine_replace ls

let shape_in_env hf ls env =
     unfs_of_ident ls hf env 
  |> (fun x -> apply_hf_in_env (hf, ls, []) x env)
  |> snd
  |> isto_of_rsto

(* MK: this is wrong; TODO Misc.fixpoint this *)
let expand_sto_shape env sto =
     CtIS.hfuns sto 
  |> List.fold_left begin fun (aps, sto) ((hf, ls, _) as app) ->
      let sto' =
        (List.hd ls |> CtIS.remove sto, shape_in_env hf ls env)
        |> M.uncurry CtIS.sh_upd in
      (app :: aps, sto') end ([], sto)

let contract_store_shapes sto hfs env =
  List.fold_left begin fun sto ((hf, ls, _) as app) ->
       unfs_of_ident ls hf env
    |> (fun x -> fold_hf_shapes_on_sto app x sto env) end sto hfs

let expand_cspec_stores cspec env =
     M.map_and_accum CtISp.map_stores begin fun acc sto ->
          expand_sto_shape env sto
       |> M.app_fst acc
       |> snd end [] (++) cspec
  |> fst
