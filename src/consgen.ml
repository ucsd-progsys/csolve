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

(* This file is part of the liquidC Project.*)

module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  W = Wrapper 
module CI = CilInterface 
module CF = Consinfra
module H  = Hashtbl

open Misc.Ops
open Cil

(* Note that Consgen -- does not -- depend on Fixpoint [Ast, Constraint]
 * All those dependencies are factored into Wrapper *)

let envt_of_fun me (genv : W.cilenv) fdec = 
  List.fold_left 
    (fun g v ->
      let vt = v.Cil.vtype in
      let vn = v.Cil.vname in
      if ST.is_ssa_name vn then W.ce_add v (W.fresh vt) g else 
        match CF.var_exp me v with 
        | Some e -> W.ce_add v (W.t_single vt e) g
        | _      -> g)
    genv fdec.Cil.slocals

let wfs_of_block me env i =
  let envi = W.ce_project env (CF.reach_vars me i) in
  let l    = CF.location me i in
  CF.ssa_targs me i 
  |> Misc.flap (fun v -> W.make_wfs envi (snd (W.ce_find v env)) l)

let cs_of_block me env i = 
  let gi = W.ce_project env ((CF.def_vars me i) ++ (CF.reach_vars me i)) in
  let p  = CF.guardp me i in
  let l  = CF.location me i in
  CF.ssa_srcs me i 
  |> Misc.flap (fun (v,vi) -> W.make_ts gi p (W.t_var vi) (snd (W.ce_find v env)) l)

let cons_of_fun genv sci =
  let me  = CF.create sci in
  let n   = Array.length sci.ST.phis in
  let env = envt_of_fun me genv sci.ST.fdec in
  (env,
   Misc.mapn (wfs_of_block me env) n |> Misc.flatten, 
   Misc.mapn (cs_of_block  me env) n |> Misc.flatten)

(* NOTE: templates for formals should be in "global" genv *)
(* API *)
let create genv scis = 
  List.fold_left 
    (fun me sci ->
      let fn             = sci.ST.fdec.Cil.svar.Cil.vname in 
  let (env, wfs, cs) = cons_of_fun genv sci in
      W.add_t me fn sci wfs cs env)
    W.empty_t scis
 
(* {{{ 
let phi_cstr cenv doms cfg i (v, bvs) = 
  let rhs = snd (W.ce_find v cenv) in
  let loc = Cil.get_stmtLoc cfg.Ssa.blocks.(i).Ssa.bstmt.skind in
  List.map 
    (fun (j,v') ->
      let lhs = W.t_var v' in 
      W.mk_cilcstr cenv doms.(j) lhs rhs loc)  
    bvs

let gen_phis cenv doms cfg phis : W.cilcstr list = 
  phis
  |> Array.mapi (fun i asgns -> Misc.tr_flap (phi_cstr cenv doms cfg i) asgns) 
  |> Array.to_list 
  |> Misc.tr_flatten


let gen_body fdec doms : A.pred = 
  let fid   = fdec.svar.vname in
  let invsr = ref [] in
  let vis   = new consGenVisitor fid doms invsr in
  let _     = visitCilFunction vis fdec in
  A.pAnd !invsr

 
let gen g sci =
  let fdec = sci.ST.fdec in
  let cfg  = sci.ST.cfg in
  let doma = guards_closure sci.ST.gdoms in
  let phis = sci.ST.phis in
  let g'   = gen_decs g  fdec in
  let invp = gen_body fdec doma in
  let ccs  = gen_phis g' doms cfg phis in
  let cs   = Misc.tr_map (W.cstr_of_cilcstr sci invp) ccs in
  cs

  }}} *)

