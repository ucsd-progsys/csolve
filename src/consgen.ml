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
module ST = Ssa_transform
module FI = FixInterface 
module CF = Consinfra

open Misc.Ops
open Cil

let mydebug = true 

let cons_fold f (env: FI.cilenv) xs =
  List.fold_left begin
    fun (env, ws, cs) x -> 
      let (env', ws', cs') = f env x in
      (env', ws' ++ ws, cs' ++ cs)
  end (env, [], []) xs


(****************************************************************************)
(********************** Constraints for Phis ********************************)
(****************************************************************************)

let extend_predenv envj vvjs =
  List.fold_left begin fun (nn's, env) (v, vj) ->
    let n   = FI.name_of_varinfo v in
    let n'  = FI.nextname_of_varinfo v in
    let cr' = vj |> FI.name_of_varinfo |> FI.t_name envj in
    ((n,n')::nn's, (FI.ce_add n' cr' env))
  end ([], envj) vvjs
  
let tcons_of_phis me phia =  
  Misc.array_flapi begin fun i asgns ->
    let asgns' = Misc.transpose asgns in
    let envi   = CF.outenv_of_block me i in
    Misc.flap begin fun (j, vvjs) ->
      let pj   = CF.guard_of_block me j in
      let locj = CF.location_of_block me j in
      let envj = CF.outenv_of_block me j in
      let nn's, envj' = extend_predenv envj vvjs in
      nn's |> Misc.flap begin fun (n, n') ->
                let lhs = FI.t_name envj' n' in
                let rhs = FI.ce_find n envi |> FI.t_subs_names nn's in
                FI.make_cs envj' pj lhs rhs locj
              end
    end asgns' 
  end phia

let bind_of_phi env v = 
  let cr = FI.t_fresh v.vtype in
  let vn = FI.name_of_varinfo v in
  (FI.ce_add vn cr env, [], [])

let wcons_of_phi loc grd env v =
  let vn = FI.name_of_varinfo v in
  let cr = FI.ce_find vn env in 
  (env, FI.make_wfs env cr loc, [])


(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let cons_of_set loc grd env (lv, e) =
  match lv with
  | (Var v), NoOffset -> 
      let cr = FI.t_exp env e in
      (FI.ce_add (FI.name_of_varinfo v) cr env, [], []) 
  | _ -> assertf "TBD: cons_of_set"

let cons_of_call loc grd env (lvo, fn, es) =
  match FI.ce_find fn env with
  | FI.Fun (ncrs, cr) ->
      asserts (List.length ncrs = List.length es) "cons_of_call: params"; 
      let ns   = Misc.map fst ncrs in
      let cenv = List.fold_left2 
                   (fun cenv n e -> FI.ce_add n (FI.t_exp env e) cenv)
                   env ns es in
      let cs   = Misc.flap 
                   (fun (n, cr) -> FI.make_cs cenv grd (FI.t_name cenv n) cr loc)
                 ncrs in
      let env' = match lvo with 
                 | None -> 
                     env  
                 | Some ((Var v), NoOffset) ->
                     let vn  = FI.name_of_varinfo v in
                     let cr' = cr |> FI.t_subs_exps (List.combine ns es) in
                     FI.ce_add vn cr' env 
                 | _  -> assertf "TBDNOW: cons_of_call" in
      (env', [], cs)
  | _ -> assertf "ERROR: cons_of_call: fname has wrong type"

let cons_of_instr loc grd env = function
  | Set (lv, e, loc) ->
      cons_of_set loc grd env (lv, e)
  | Call (lvo, Lval ((Var fv), NoOffset), es, loc) ->
      cons_of_call loc grd env (lvo, (FI.name_of_varinfo fv), es)  
  | i -> 
      E.warn "cons_of_instr: %a \n" d_instr i;
      assertf "TBD: cons_of_instr"

(****************************************************************************)
(********************** Constraints for [stmt] ******************************)
(****************************************************************************)

let cons_of_ret loc fn grd env e =
  match FI.ce_find fn env with
  | FI.Fun (_, cr) ->
      (env, [], FI.make_cs env grd (FI.t_exp env e) cr loc) 
  | _ ->
      assertf "ERROR:cons_of_ret"

let cons_of_stmt loc fn grd env stmt = 
  match stmt.skind with
  | Instr is -> 
      cons_fold (cons_of_instr loc grd) env is
  | Return ((Some e), _) ->
      cons_of_ret loc fn grd env e
  | _ ->
      (env, [], [])

(****************************************************************************)
(********************** Constraints for (cfg)block **************************)
(****************************************************************************)

let cons_of_block me i =
  let grd           = CF.guard_of_block me i in
  let loc           = CF.location_of_block me i in
  let phis          = CF.phis_of_block me i in
  let fn            = CF.fname me in 
  let env           = CF.inenv_of_block me i in
  let env, _  , _   = cons_fold bind_of_phi env phis in 
  let env, ws1, cs1 = cons_fold (wcons_of_phi loc grd) env phis in
  let env, ws2, cs2 = cons_of_stmt loc fn grd env (CF.stmt_of_block me i) in
  (env, ws1 ++ ws2, cs1 ++ cs2)

(****************************************************************************)
(********************** Constraints for ST.ssaCfgInfo ***********************)
(****************************************************************************)

let process_block me i = 
  let env, ws, cs = cons_of_block me i in
  me |> CF.add_env i env
     |> CF.add_cons ws cs

let process_phis phia me =
  let cs = tcons_of_phis me phia in
  CF.add_cons [] cs me 

let cons_of_sci gnv sci =
  let _ = if !Constants.ctypes then
    let (ctm, _) = Inferctypes.infer_sci_shapes sci in
    let (bs, th) = (Refanno.annotate_cfg sci.ST.cfg ctm) in
    let _ = Array.iter (fun b -> Refanno.print_block_anno b) bs in
    Refanno.print_ctab th in
  CF.create gnv sci
  |> Misc.foldn process_block (Array.length sci.ST.phis)
  |> process_phis sci.ST.phis 
  |> CF.get_cons

(************************************************************************************)
(***************** Processing SCIs **************************************************)
(************************************************************************************)

let add_scis gnv scis ci = 
  List.fold_left begin 
    fun ci sci ->
      let fn = sci.ST.fdec.Cil.svar.Cil.vname in
      cons_of_sci gnv sci 
      |> Misc.uncurry (Consindex.add ci fn sci)
  end ci scis

let scis_of_file cil = 
  Cil.foldGlobals cil
    (fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> 
          let sci = ST.fdec_to_ssa_cfg fdec loc in
          let _   = if mydebug then ST.print_sci sci in
          sci::acc
      | _ -> acc) [] 

(************************************************************************************)
(***************** Processing Globals ***********************************************)
(************************************************************************************)

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
          let t  = type_of_fdec fdec in
          let fn = FI.name_of_varinfo fdec.svar in
          FI.ce_add fn (FI.t_fresh t) gnv  
      | _ -> 
          ignore (E.warn "Ignoring global: %a \n" d_global g);
          gnv
  end FI.ce_empty 

let cons_of_globals gnv cil = 
  Cil.foldGlobals cil begin
    fun (ws, cs) g ->
      match g with
      | Cil.GFun (fdec, loc) ->
          let fn = FI.name_of_varinfo fdec.svar in
          let cr = FI.ce_find fn gnv in
          ((FI.make_wfs gnv cr loc) ++ ws, cs)
      | _ -> 
          E.warn "Ignoring global %a \n" d_global g;
          (ws, cs)

  end ([], []) 

(************************************************************************************)
(******************************** API ***********************************************)
(************************************************************************************)

(* API *)
let create (cil: Cil.file) = 
  let gnv = gnv_of_file cil in
  cons_of_globals gnv cil
  |> Misc.uncurry Consindex.create
  |> add_scis gnv (scis_of_file cil)  
