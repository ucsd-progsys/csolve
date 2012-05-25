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
module E  = ErrorMsg
module P  = Pretty
module M  = FixMisc
module I  = Index
module Sl = Sloc
module RA = Refanno
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

let acp = Array.copy
let hcp = Hashtbl.copy

type unf  = Sl.t list list
type appm = (Sl.t * Ct.ind_hf_appl * unf) SLM.t

let hf_of_appm l =
  SLM.find l |> snd3 |> fst3

let ls_of_appm l =
  SLM.find l |> snd3 |> snd3

let unf_of_appm l =
  SLM.find l |> thd3

let cnc_of_appm l =
  SLM.find l |> fst3

type me = (string, Sl.t) Hashtbl.t * (string, Sl.t) Hashtbl.t

let cl_of_me = snd
let al_of_me = fst

let set_cl me =
  me 
  |> cl_of_me
  |> Hashtbl.replace 

let maybe_set_cl me = function
  | Some cl -> set_cl me cl
  | _ -> ()

let set_al me =
  me
  |> al_of_me
  |> Hashtbl.replace

let maybe_set_al me = function
  | Some al -> set_al me al
  | _ -> ()

let get_al me =
  me
  |> al_of_me
  |> M.hashtbl_maybe_find

let get_cl me =
  me
  |> cl_of_me
  |> M.hashtbl_maybe_find

let expr_has_ptr_type ctm e =
  try
    match CtI.ExpMap.find e ctm with
    | Ct.Ref _ -> true
    | _        -> false
  with Not_found -> false

let ind_of_expr ctm e =
  try
    match Ctypes.I.ExpMap.find e ctm with
    | Ctypes.Ref (_, i) -> Some i 
    | _                 -> None
  with Not_found -> Pretty.printf "(Could not find %a\n\n" Cil.d_exp e;
  assert false

let set_sl sm l v =
  let _ = assert not (Hashtbl.mem sm v) in
  Hashtbl.replace sm v l 

let instantiate_cnc sto sm icl rw =
  let scl               = Sl.copy_concrete icl in
  let sto'              = CtIS.find sto icl |> CtIS.add sto scl in
  let _                 = set_sm sm v scl in
  (RA.Ins (icl, scl), sto', Some scl)

let instantiate_hf sto sm am icl rw =
  let env               = Hf.test_env in
  let (hf, _, _) as app = Hf.binding_of icl hfs in
  let ins               = Hf.fresh_ins hf env in
  let (dep, sto')       = Hf.apply_hf_in_env app ins env in
  let sto''             = Hf.apply_hf_sto l sto' sto in
  let scl               = CtIS.domain sto' |> M.ex_one in
  let _                 = set_sm sm v scl
  let _                 = set_am am scl (app, ins)
  (RA.HIns (icl, ins), sto'', Some scl)

let is_deref = function
  | LVal (Mem _, _) -> true
  | _ -> false

let al_of_expr sto gst ctm me appm e =
  let _   = assert (not is_deref e) in
  let v   = CM.referenced_var_of_expr e in
  let icl = RA.sloc_of_expr ctm e in
  let scl = M.hashtbl_maybe_find sm v in
  match icl, scl with
  | None, _            -> None
  | Some icl, Some scl -> Some scl
  | Some icl, None     -> set_al me icl; Some icl
  | _ -> assert false

let cl_of_expr sto gst ctm me appm = function
  | LVal (Var v, _) -> get_cl me v
  | _ -> None

(*  match icl, scl with
  | Some icl, Some scl ->
      ([], sto, Some scl)
  | Some icl, None   when CtIS.hfuns sto |> Hf.binds icl ->
      instantiate_hf sto sm am icl
  | Some icl, None   when not (CtI.mem sto icl) ->
      generalize_hf sto sm am icl
      |> instantiate_hf sto sm am icl
  | Some icl, None   when CtIS.mem sto icl ->
      instantiate_cnc sto sm icl
  | _ -> ([], sto, None)
  *)

let sloc_of_deref sto ctm me appm em =
  match al_of_expr sto ctm sm em with
    | Some l -> 
        let i = ind_of_expr ctm em in
        match CtIS.find sto l |> CtIL.find i with
        | [(_, fld)] -> CtIF.sloc_of fld
        | _          -> assert false
    | None   -> assert false

let abs_of_conc appm l =
  SLM.filter (fun x y -> 

let slocs_of_deref sto ctm me appm em =
  match sloc_of_deref sto ctm me appm em with
  | None ->  (None, None) 
  | Some l when Sl.is_conc l ->
      (abs_of_conc appm l, Some l)
  | Some l with Sl.is_abs l  -> (Some l, None)
  | _ -> assert false

let annotate_instr sto ctm gst annom = function 
  | C.Set s  -> annotate_set sto gst annom s
  | C.Call _ -> assert false
  | C.Call _ -> assert false
  | i -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

  (* val annotate_instr : Ct.store ->
                          Ct.store ->
                          annom ->
                          C.instr ->
                          Ct.store * Ct.store * annom *)

let annotate_read sto ctm gst annom = function
  | Lval (Mem e) ->
    (* unfold, please cnc into var_of e *)
  | e ->
    let v = CM.referenced_var_of e in
    let l = sloc_of_v sm v in
    match l with
    | Some l -> (Dirty, l)
    | None   -> (Clean, l)
    (* pass back a location to propogate unfolding info *)

let annotate_set sto gst ctm me appm (lv, e, _) =
  match lv, e with
  (* v := *v1 *)
  | (C.Var v, _), Lval (C.Mem e, _) ->
    let al  = al_of_expr ctm me e |> M.maybe in 
    let (anno, sto, gst, me, appm) =
      annotate_read sto gst ctm me appm e in
    let (dal, dcl) = slocs_of_deref sto gst me appm e in
    let _ = set_al me v dal; maybe_set_cl me v dcl in
    (anno, sto, gst, me, appm)
  (* v := e *)
  | (C.Var v, _), e ->
    let _ = al_of_expr ctm me e |> maybe_set_al me v in
    let _ = cl_of_expr ctm me e |> maybe_set_cl me v in
    ([], sto, gst, me, appm)
  | (C.Mem e1, _), LVal (C.Mem e, _) -> assert false
  | (C.Mem e1, _), e2, _ -> assert false


let annotate_block j sto gst annom =

  (* val annotate_block : int ->
                          Ct.store ->
                          Ct.store ->
                          annom ->
                          ranno ->
                          instr ->
                          Ct.store * Ct.store * annom * ranno *)


(*let annot_iter cfg sh globalslocs anna =
  let do_block j (_, ans) =
    match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
    | Instr is -> annotate_block sto globalslocs ctm theta j anna.(j) is
    | _        -> ans in
  Misc.array_fold_lefti do_block [] sol*)

let annotate_cfg cfg gst shp =
  let anna, conca, theta, sto, ctm = acp shp.anna, acp shp.conca,
                                     hcp shp.theta, shp.sto, shp.ctemap in
  let sm = Hashtbl.create 16 in
  annot_iter  
