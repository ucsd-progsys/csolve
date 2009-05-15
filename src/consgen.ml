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
module E  = Errormsg
module SM = Misc.StringMap
module ST = Ssa_transform
module  W = Wrapper 
module CI = CilInterface 
module CF = Consinfra
module H  = Hashtbl
module Co = Constants
open Misc.Ops
open Cil

(* Note that Consgen -- does not -- depend on Fixpoint [Ast, Constraint]
 * All those dependencies are factored into Wrapper *)

let envt_of_fun me (genv : W.cilenv) fdec = 
  fdec.Cil.slocals ++ fdec.Cil.sformals
  |> List.fold_left 
      (fun g v ->
        let vt = v.Cil.vtype in
        let vr = match CF.var_exp me v with
                 | CF.Exp e -> W.t_single vt e 
                 | CF.Phi   -> W.fresh vt 
                 | CF.Undef -> W.t_true vt in
        W.ce_add v vr g)
      genv 

let wfs_of_block me env i =
  let vsi  = CF.reach_vars me i in
  let envi = W.ce_project env vsi in
  let loc  = CF.location me i in
  CF.ssa_targs me i 
  |> Misc.flap (fun v -> W.make_wfs envi (W.ce_find v env) loc)

let cs_of_block me env i =
  let vsi = CF.def_vars me i ++ CF.reach_vars me i in
  let gi  = W.ce_project env vsi in
  let p   = CF.guardp me i in
  let loc = CF.location me i in
  CF.ssa_srcs me i 
  |> Misc.flap (fun (v,vi) -> W.make_ts gi p (W.t_var vi) (W.ce_find v env) loc) 

let cons_of_fun genv sci =
  let _   = Inferctypes.infer_sci_shapes sci in
  let me  = CF.create sci in
  let n   = Array.length sci.ST.phis in
  let env = envt_of_fun me genv sci.ST.fdec in
  let _   = F.printf "env_of_fun:@   @[%a@] @ " (W.print_ce None) env in
  (env,
   Misc.mapn (wfs_of_block me env) n |> Misc.flatten,
   Misc.mapn (cs_of_block  me env) n |> Misc.flatten)


(* NOTE: 1. templates for formals are in "global" genv, 
         2. each function var is bound to its "output" *) 
let genv_of_file cil =                       
  Cil.foldGlobals cil begin
    fun genv g ->
      match g with
      | GFun (fdec, _) ->
          let rett = match fdec.svar.vtype with TFun (t,_,_,_) -> t 
                                              | _ -> assertf "genv_of_file" in
          let ret  = fdec.svar, W.fresh rett in
          let fmls = Misc.map (fun v -> (v, W.fresh v.vtype)) fdec.sformals in
          List.fold_left (fun ce (v, r) -> W.ce_add v r ce) genv (ret :: fmls) 
      | _ -> 
          ignore (E.warn "Ignoring global: %a \n" d_global g);
          genv
  end W.ce_empty 

let scis_of_file cil = 
  Cil.foldGlobals cil
    (fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> 
          let sci = ST.fdec_to_ssa_cfg fdec loc in
          let _   = if Co.ck_olev Co.ol_insane then ST.print_sci sci in
          sci::acc
      | _ -> acc) [] 

(* API *)
let create (cil: Cil.file) = 
  let genv = genv_of_file cil in
  let scis = scis_of_file cil in
  List.fold_left 
    (fun me sci ->
      let fn             = sci.ST.fdec.Cil.svar.Cil.vname in
      let (env, wfs, cs) = cons_of_fun genv sci in
      W.add_t me fn sci wfs cs env)
    W.empty_t scis
