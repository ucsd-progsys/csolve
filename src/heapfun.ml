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
module SM = M.StringMap
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
module CtISp = CtI.Spec
module CtICF = CtI.CFun

open M.Ops

(* Heap Functions. Let us assume that exactly the first location argument is bound *)

module VR  = Ct.VarRefinement
module RVT = Ct.RefVarTypes
module RVS = Ct.Make(RVT)
module RVSS = RVS.Store
module RVSF = RVS.Field
module RVSC = RVS.CType
module RVSL = RVS.LDesc

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

type hfspec  = CtISp.t

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
  let nrf = ("", I.top) in
  let nr0 = ("", I.of_int 0) in
  let nm = "intlist" in
  let l  = Sl.fresh_abstract (CM.srcinfo_of_string "intlist_def") in
  let (lj, l') = (Sl.copy_concrete l, Sl.copy_fresh l) in
  let ld = RVSL.create {Ct.stype = None; Ct.any = false} 
    [(I.of_int 0, RVSF.create Ct.Final    Ct.dummy_fieldinfo (Ct.Int(4, nrf)));
     (I.of_int 4, RVSF.create Ct.Nonfinal Ct.dummy_fieldinfo (Ct.Ref(l', nr0)))] in
  let ap = (nm, [l'], []) in
  { loc_params = [l]
  ; ref_params = []
  ; unfolds    = [[lj; l']]
  ; rhs        = RVSS.add RVSS.empty lj ld |> M.flip RVSS.add_app ap
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

let apply_hf_in_env ((hf, _, _) as app) ins env =
     HfMap.find hf env
  |> apply_hf_shape app ins

  (* remove dependencies *)
(* in the current implementation we're going to assume
 * that the only argument that can be bound to an ldesc
 * in the expansion of an hfun is the first argument.
 * BUT, any introduced location could be bound to an
 * ldesc. *)
let fold_hf_on_sto ((hf, ls, _) as app) ins sto env =
  let bound_locs     = List.hd ls :: List.flatten ins in
  let bound_by_exp x = List.mem x bound_locs in
  let (exp, sto)     = CtIS.partition bound_by_exp sto in
  let desired_exp    = apply_hf_in_env app ins env |> snd in
  (*let _              = asserts (exp = desired_exp) "fold_hf_on_sto" in*)
  (* MK: 1) there should be a Store.eq,
   *     2) this should be heap subtyping *)
     sto
  |> M.flip CtIS.add_app app

let ins l ls ins deps sto env =
  let (hf, ls', _) =
       CtIS.hfuns sto
    |> Ct.hf_appl_binding_of l
    |> M.maybe in
  apply_hf_in_env (hf, ls, []) ins env
  |> M.app_fst (SlSS.union deps)
  |> M.app_snd (CtIS.remove sto l |> CtIS.upd)


let gen ((hf, ls, _) as app) ins deps sto env =
  let deps = M.combine_prefix ls ins |>
             List.fold_left begin fun deps ll ->
               try SlSS.remove ll deps with Not_found -> deps end deps in
  let sto  = fold_hf_on_sto (hf, ls, []) ins sto env in
  deps, sto

let unfs_of_ident ls hf env =
     HfMap.find hf env
  |> unfs_of
  |> M.combine_replace ls

let shape_in_env hf ls env =
     unfs_of_ident ls hf env 
  |> (fun x -> apply_hf_in_env (hf, ls, []) x env)
  |> snd

(* MK: this is wrong; TODO Misc.fixpoint this *)
let expand_sto_shape sto env =
     CtIS.hfuns sto 
  |> List.fold_left begin fun (aps, sto) ((hf, ls, _) as app) ->
      let sto' =
        (List.hd ls |> CtIS.remove sto, shape_in_env hf ls env)
        |> M.uncurry CtIS.sh_upd in
      (app :: aps, sto') end ([], sto)

let contract_store sto hfs env =
  List.fold_left begin fun sto ((hf, ls, _) as app) ->
       unfs_of_ident ls hf env
    |> (fun x -> fold_hf_on_sto app x sto env) end sto hfs

let split_cspec_stores g cspec env =
  CtISp.map_stores (fun x -> expand_sto_shape x env |> g) cspec

(* MK: hfs are NOT order-invariant. they are a recording of expansions made *)
  (* this should actually return an opaque type so the invariant can be
   * maintained *)
let expand_cspec_stores cspec env =
  let hfspec =
    split_cspec_stores (fun x -> fst x |> CtIS.sto_of_hfs) cspec env in
  let exspec =
    split_cspec_stores snd cspec env in
  hfspec, exspec

let hfs_of_fun_in_hfspec hfspec fn =
     CtISp.find_cf hfspec fn
  |> fst
  |> CtICF.sto_in
  |> CtIS.hfuns
