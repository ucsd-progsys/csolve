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

let set_cl me v =
  cl_of_me me
  |> Hashtbl.replace
  <| v.vname

let set_al me v =
  al_of_me me
  |> Hashtbl.replace
  <| v.vname

let maybe_set_cl me v = function
  | Some cl -> set_cl me v cl
  | _ -> ()

let maybe_set_al me v = function
  | Some al -> set_al me v al
  | _ -> ()

let get_al me v =
  al_of_me me
  |> M.hashtbl_maybe_find
  <| v.vname
    
let get_cl me v =
  cl_of_me me
  |> M.hashtbl_maybe_find
  <| v.vname

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

let generalize sto gst ctm me appm al =
  assert false

let instantiate sto gst ctm me appm al cl = 
  assert false 

let instantiate_cnc sto sm icl rw =
  assert false
(*  let scl               = Sl.copy_concrete icl in
  let sto'              = CtIS.find sto icl |> CtIS.add sto scl in
  let _                 = set_sm sm v scl in
  (RA.Ins (icl, scl), sto', Some scl)
  *)

let instantiate_hf sto sm am icl rw =
  assert false
(*  let env               = Hf.test_env in
  let (hf, _, _) as app = Hf.binding_of icl hfs in
  let ins               = Hf.fresh_ins hf env in
  let (dep, sto')       = Hf.apply_hf_in_env app ins env in
  let sto''             = Hf.apply_hf_sto l sto' sto in
  let scl               = CtIS.domain sto' |> M.ex_one in
  let _                 = set_sm sm v scl
  let _                 = set_am am scl (app, ins)
  (RA.HIns (icl, ins), sto'', Some scl)
  *)

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
  if SLM.mem al appm then
    begin
      if SLM.find al appm |> concretizes cl then
        ([], sto, gst, me, appm)
      else
        let (ann1, sto, gst, me, appm) = generalize sto gst ctm me appm al in
        let (ann2, sto, gst, me, appm) = instantiate sto gst ctm me appm al cl in
        (ann1 ++ ann2, sto, gst, me, appm)
    end
  else
    instantiate sto gst ctm me appm al cl

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

  | (Mem e1, _), Lval (Mem e, _) -> assert false
  | (Mem e1, _), e2 -> assert false

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
  assert false
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

let annotate_cfg cfg shp =
  assert false
(*  let anna, conca, theta, sto, ctm = acp shp.anna, acp shp.conca,
                                     hcp shp.theta, shp.sto, shp.ctemap in
  let sm = Hashtbl.create 16 in
  annot_iter  *)
