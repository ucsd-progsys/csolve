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

let mydebug = false

(* Note that Consgen -- does not -- depend on Fixpoint [Ast, Constraint]
 * All those dependencies are factored into Wrapper *)

let envt_of_fun me fdec = 
  fdec.Cil.slocals
  |> List.fold_left 
      (fun env v ->
        let vt = v.Cil.vtype in
        let vr = match CF.var_exp me v with
                 | CF.Exp e -> W.t_single vt e 
                 | CF.Phi   -> W.t_fresh vt 
                 | CF.Undef -> W.t_true vt in
        W.ce_add v vr env)
      W.ce_empty

let wfs_of_block me gnv env i =
  let vsi  = CF.reach_vars me i in
  let envi = W.ce_project gnv env vsi in
  let loc  = CF.location me i in
  CF.ssa_targs me i 
  |> Misc.flap (fun v -> W.make_wfs envi (W.ce_find v env) loc)

let cs_of_block me gnv env i =
  let vsi  = CF.def_vars me i ++ CF.reach_vars me i in
  let envi = W.ce_project gnv env vsi in
  let p    = CF.guardp me i in
  let loc  = CF.location me i in
  CF.ssa_srcs me i 
  |> Misc.flap (fun (v,vi) -> W.make_ts envi p (W.t_var vi) (myfind v env) loc) 

let cons_of_fun gnv sci =
  let _    = Inferctypes.infer_sci_shapes sci in
  let me   = CF.create sci in
  let n    = Array.length sci.ST.phis in
  let (gnv,_) = W.ce_unroll sci.ST.fdec.svar gnv in
  let _    = Co.bprintf true "gnv_of_fun:@   @[%a@] @ " (W.print_ce None) gnv in
  let env  = envt_of_fun me sci.ST.fdec in
  let _    = Co.bprintf true "env_of_fun:@   @[%a@] @ " (W.print_ce None) env in
  (env,   
   Misc.mapn (wfs_of_block me gnv env) n |> Misc.flatten,
   Misc.mapn (cs_of_block  me gnv env) n |> Misc.flatten)

let type_of_fdec fdec = 
  let fn = fdec.svar.vname in
  match fdec.svar.vtype with 
  | TFun (a,Some xts, b, c) ->
      let xts' = List.map (fun (x,d,e) -> (x^"@"^fn, d, e)) xts in
      TFun (a, Some xts', b, c)
  | TFun (_,_,_,_) as t -> 
      t
  | _  -> 
      assertf "type_of_fdec"

(* NOTE: 1. templates for formals are in "global" gnv, 
         2. each function var is bound to its "output" *) 
let gnv_of_file cil =                       
  Cil.foldGlobals cil begin
    fun gnv g ->
      match g with
      | GFun (fdec, _) ->
          let t = type_of_fdec fdec in
          W.ce_add fdec.svar (W.t_fresh t) gnv  
      | _ -> 
          ignore (E.warn "Ignoring global: %a \n" d_global g);
          gnv
  end W.ce_empty 

let scis_of_file cil = 
  Cil.foldGlobals cil
    (fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> 
          let sci = ST.fdec_to_ssa_cfg fdec loc in
          let _   = if false then ST.print_sci sci in
          sci::acc
      | _ -> acc) [] 

let add_scis gnv scis me = 
  List.fold_left 
    (fun me sci ->
      let fn             = sci.ST.fdec.Cil.svar.Cil.vname in
      let (env, wfs, cs) = cons_of_fun gnv sci in
      W.add_t me fn sci wfs cs env)
  me scis

let cons_of_globals gnv cil = 
  Cil.foldGlobals cil begin
    fun (ws, cs) g ->
      match g with
      | Cil.GFun (fdec, loc) ->
          let ws' = (W.make_wfs gnv (W.ce_find fdec.svar gnv) loc) ++ ws in
          (ws', cs)
      | _ -> assertf "add_globals"
  end ([], []) 

(* API *)
let create (cil: Cil.file) = 
  let gnv = gnv_of_file cil in
  let scis = scis_of_file cil in
  cons_of_globals gnv cil
  |> Misc.uncurry W.create_t
  |> add_scis gnv scis 
