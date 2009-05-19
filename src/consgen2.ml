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
module W  = Wrapper 
module CF = Consinfra2

open Misc.Ops
open Cil

let mydebug = false

(* Note that Consgen -- does not -- depend on Fixpoint [Ast, Constraint]
 * All those dependencies are factored into Wrapper *)

let tcons_of_phis me phia =  
  Misc.array_flapi begin fun i asgns ->
    Misc.flap begin fun (v, srcs) ->
      Misc.flap begin fun (j, vj) -> 
        let envj = CF.outenv_of_block me j in
        let pj   = CF.guard_of_block me j in
        let locj = CF.location_of_block me j in
        let lhs  = vj |> W.name_of_varinfo |> W.t_name envj in
        let rhs  = W.ce_find (W.name_of_varinfo v) (CF.outenv_of_block me i) in
        W.make_ts envj pj lhs rhs locj
      end srcs
    end asgns
  end phia

let wcons_of_phi loc env grd v =
  let cr = W.t_fresh v.vtype in
  let vn = W.name_of_varinfo v in
  (W.ce_add vn cr env, W.make_wfs env cr loc, [])

let cons_of_set loc env grd lv e =
  match lv with
  | (Var v), NoOffset -> 
      let cr = W.t_exp env v.vtype e in
      (W.ce_add (W.name_of_varinfo v) cr env, [], []) 
  | _ -> assertf "TBD: cons_of_set"

let cons_of_call loc env grd lvo e es =
  failwith "TBDNOW: cons_of_call"
  (*
  let cs = ... in
  let cr = ... in
  match lvo with
  | None                     -> (env, [], cs)
  | Some ((Var v), NoOffset) -> (W.ce_add v cr env, [], cs)
*)

let cons_of_instr loc env grd = function
  | Set (lv, e, loc) ->
      cons_of_set loc env grd lv e
  | Call (lvo, e, es, loc) ->
      cons_of_call loc env grd lvo e es  
  | _ -> 
      assertf "TBD: cons_of_instr"

let cons_of_ret loc env grd e = 
  E.warn "TBDNOW: cons_of_ret";
  (env, [], [])

let cons_fold f (env: W.cilenv) grd xs =
  List.fold_left begin
    fun (env, ws, cs) x -> 
      let (env', ws', cs') = f env grd x in
      (env', ws' ++ ws, cs' ++ cs)
  end (env, [], []) xs

let cons_of_stmt loc (env : W.cilenv) grd stmt = 
  match stmt.skind with
  | Instr is -> 
      cons_fold (cons_of_instr loc) env grd is
  | Return ((Some e), _) ->
      cons_of_ret loc env grd e
  | _ ->
      (env, [], [])

let cons_of_block me i =
  let env0             = CF.inenv_of_block me i in
  let grd              = CF.guard_of_block me i in
  let loc              = CF.location_of_block me i in
  let (env1, ws1, cs1) = cons_fold (wcons_of_phi loc) env0 grd (CF.phis_of_block me i) in
  let (env2, ws2, cs2) = cons_of_stmt loc env1 grd (CF.stmt_of_block me i) in
  (env2, ws1 ++ ws2, cs1 ++ cs2)

let process_block me i = 
  let env, ws, cs = cons_of_block me i in
  me |> CF.add_env i env
     |> CF.add_cons ws cs

let process_phis phia me =
  let cs = tcons_of_phis me phia in
  CF.add_cons [] cs me 

let cons_of_sci gnv sci =
  let _    = Inferctypes.infer_sci_shapes sci in
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
          let _   = if false then ST.print_sci sci in
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
          let fn = W.name_of_varinfo fdec.svar in
          W.ce_add fn (W.t_fresh t) gnv  
      | _ -> 
          ignore (E.warn "Ignoring global: %a \n" d_global g);
          gnv
  end W.ce_empty 

let cons_of_globals gnv cil = 
  Cil.foldGlobals cil begin
    fun (ws, cs) g ->
      match g with
      | Cil.GFun (fdec, loc) ->
          let fn = W.name_of_varinfo fdec.svar in
          let cr = W.ce_find fn gnv in
          ((W.make_wfs gnv cr loc) ++ ws, cs)
      | _ -> assertf "add_globals"
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
