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

module E  = Errormsg
module P  = Pretty
module M  = FixMisc
module I  = Index
module Sl = Sloc
module RA = Refanno
module Ct = Ctypes
module FC = FixConstraint
module CM = CilMisc
module Hf = Heapfun
module SlS  = Sl.Subst
module SlSS = Sl.SlocSlocSet
module SLM  = Sl.SlocMap
module CtI  = Ct.I
module CtIS = CtI.Store
module CtIF = CtI.Field
module CtIC = CtI.CType
module CtIL = CtI.LDesc
module CtISp = CtI.Spec
module CtICF = CtI.CFun

open Cil
open M.Ops

let acp = Array.copy
let hcp = Hashtbl.copy

type unf  = Sl.t list list
type app  =
  | App of Sl.t * Ct.ind_hf_appl * unf
  | Cnc of Sl.t

let concretizes l = function
  | Cnc (l') -> Sloc.eq l l'
  | App (_, _, unf) ->
      List.flatten unf
      |> M.list_contains Sl.eq l

type appm = app SLM.t

type me = (string, Sl.t) Hashtbl.t * (string, Sl.t) Hashtbl.t

let cl_of_me = snd
let al_of_me = fst
let get_l me v = v.vname |> M.hashtbl_maybe_find me
let set_l me v = v.vname |> Hashtbl.replace me
let get_al me = al_of_me me |> get_l
let get_cl me = cl_of_me me |> get_l
let set_cl me = cl_of_me me |> set_l
let set_al me = al_of_me me |> set_l

let maybe_set_cl me v = function
  | Some cl -> set_cl me v cl
  | _ -> ()

let maybe_set_al me v = function
  | Some al -> set_al me v al
  | _ -> ()

let always_get_cl me al v =
  match get_cl me v with
  | Some l -> l
  | None -> let l = Sl.copy_concrete al in
      begin set_cl me v l; l end
  
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

let is_unf appm al = try
    SLM.find al appm
    |> (function App(cl, _, _) -> Some cl 
               | Cnc cl        -> Some cl)
  with Not_found -> None 

let is_unf_by appm al cl =
  is_unf appm al
  |> function Some cl' -> Sl.eq cl cl' | None -> false

let generalize_cnc sto al cl =
  ([RA.Gen (cl, al)], CtIS.remove sto cl)

let generalize_hf sto cl (hf, ls, _) ins env =
  let sto' = Hf.fold_hf_on_hp ls ins sto hf env in
  ([RA.HGen (hf, ls, ins)], sto')

let generalize sto gst ctm me appm al =
  let _   = assert (SLM.mem al appm) in 
  let env = Hf.test_env in
  let (ann, sto) = match SLM.find al appm with
  | App (cl, app, ins)  -> generalize_hf sto cl app ins env
  | Cnc cl              -> generalize_cnc sto al cl in
  (ann, sto, gst, me, SLM.remove al appm)

let instantiate_cnc sto me appm v al =
  let cl = Sl.copy_concrete al in
  let _  = set_cl me v cl in
  let sto' = CtIS.find sto al |> CtIS.add sto cl in
  ([RA.Ins (v.vname, cl, al)], sto', SLM.add al (Cnc cl) appm)

let instantiate_hf sto appm me env v al cl =
  let (hf, _, _) as app = CtIS.hfuns sto |> Hf.binding_of al in
  let ins               = Hf.fresh_unfs_of_hf cl hf env in
  let (dep, sto')       = Hf.apply_hf_in_env app ins env in
  let _                 = set_cl me v cl in
  let appm'             = SLM.add al (App(cl, app, ins)) appm in
  ([RA.HIns (cl, ins)], sto', appm)

let instantiate_aux sto gst ctm me appm v al cl = 
  let hf_env = Hf.test_env in
  begin if CtIS.hfuns sto |> Hf.binds al then
    instantiate_hf sto appm me hf_env v al cl
  else
     instantiate_cnc sto me appm v al end
  |> (fun (ann, sto, appm) -> (ann, sto, gst, me, appm))

let instantiate sto gst ctm me appm v al cl =
  let (ann, sto, gst, me, appm) =
    if is_unf_by appm al cl then
      generalize sto gst ctm me appm al
    else ([], sto, gst, me, appm) in
  instantiate_aux sto gst ctm me appm v al cl

let is_deref = function
  | Lval (Mem _, _) -> true
  | _ -> false

let ctm_sloc_of_expr ctm e =
  try
    match CtI.ExpMap.find e ctm with
    | Ct.Ref (s, _) -> Some s
    | _             -> None
  with Not_found -> Pretty.printf "Could not find %a\n\n" d_exp e; assert false

let al_of_expr ctm me e =
  let _   = assert (not (is_deref e)) in
  let v   = CM.referenced_var_of_exp e in
  let icl = ctm_sloc_of_expr ctm e in
  let scl = get_al me v in
  match icl, scl with
  | None, _            -> None
  | Some icl, Some scl -> Some scl
  | Some icl, None     -> set_al me v icl; Some icl
  | _ -> assert false

let cl_of_expr me = function
  | Lval (Var v, _) -> get_cl me v
  | _ -> None

let possibly_fresh_cl_of_expr me = function
  | Lval (Var v, _) ->
    get_al me v
    |> M.maybe
    |> (fun x -> always_get_cl me x v)
  | _ -> assert false

let abs_of_conc_from_app l = function
  | (al, Cnc _) -> al  
  | (_, App (_, (_, ls, _), un)) ->
      List.combine ls un
      |> List.find (fun (al, ls) -> M.list_contains Sl.eq l ls)
      |> fst

let abs_of_conc l appm =
  SLM.to_list appm
  |> List.filter (fun (_, y) -> concretizes l y)
  |> M.ex_one "abs_of_conc: more than one aloc corresponding to cloc"
  |> abs_of_conc_from_app l

let sloc_of_deref sto gst ctm me appm em =
  match al_of_expr ctm me em with
    | Some l -> 
        let i = ind_of_expr ctm em |> M.maybe in
        begin match CtIS.find sto l |> CtIL.find i with
        | [(_, fld)] -> CtIF.sloc_of fld
        | _          -> assert false end
    | None   -> assert false

let slocs_of_deref sto gst ctm me appm em =
  match sloc_of_deref sto gst ctm me appm em with
  | None ->  (None, None) 
  | Some l when Sl.is_concrete l ->
      (Some (abs_of_conc l appm), Some l)
  | Some l when Sl.is_abstract l  ->
      (Some l, None)
  | _ -> assert false

let annotate_read sto gst ctm me appm e =
  let v  = CM.referenced_var_of_exp e in
  let al = al_of_expr ctm me e |> M.maybe in
  let cl = possibly_fresh_cl_of_expr me e in
    instantiate sto gst ctm me appm v al cl

let annotate_set sto gst ctm me appm = function
  (* v := *v1 *)
  | (Var v, _), Lval (Mem e, _) ->
    let (anno, sto, gst, me, appm) =
      annotate_read sto gst ctm me appm e in
    let (dal, dcl) = slocs_of_deref sto gst ctm me appm e in
    let _ = begin dal |> M.maybe |> set_al me v end
            ; maybe_set_cl me v dcl in
    (anno, sto, gst, me, appm)

  (* v := e *)
  | (Var v, _), e ->
    let _ = al_of_expr ctm me e |> maybe_set_al me v in
    let _ = cl_of_expr me e     |> maybe_set_cl me v in
    ([], sto, gst, me, appm)

  | (Mem e1, _), e2 when expr_has_ptr_type ctm e2 ->
      let al, cl = al_of_expr me e2, possibly_fresh_cl_of_expr me e2 in
      let (anno, sto, gst, me, appm) =
        annotate_write 

  | (Mem e1, _), _ ->
      annotate_write sto gst me appm e1

  | lv, e ->
      ErrorMsg.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let annotate_instr sto gst ctm me appm = function 
  | Set (lv, e, _) -> annotate_set sto gst ctm me appm (lv, e)
  | Call _         -> assert false
  | Call _         -> assert false
  | i -> E.s <| bug "Unimplemented constrain_instr: %a@!@!" dn_instr i

  (* val annotate_instr : Ct.store ->
                          Ct.store ->
                          annom ->
                          C.instr ->
                          Ct.store * Ct.store * annom *)

let annotate_block j sto gst annom =
   
  (* val annotate_block : int ->
                          Ct.store ->
                          Ct.store ->
                          annom ->
                          ranno ->
                          instr ->
                          Ct.store * Ct.store * annom * ranno *)

let fold_all appm = assert false

let annot_iter cfg sh globalslocs anna =
  let do_block j (_, ans) =
    match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
    | Instr is -> annotate_block sto globalslocs ctm theta j anna.(j) is
    | _        -> ans in
  Misc.array_fold_lefti do_block [] sol

let annotate_cfg cfg shp =
  let anna, conca, theta, sto, ctm = acp shp.anna, acp shp.conca,
                                     hcp shp.theta, shp.sto, shp.ctemap in
  let me = (Hashtbl.create 16, Hashtbl.create 16)
  annotate_iter anna ctm sto me
