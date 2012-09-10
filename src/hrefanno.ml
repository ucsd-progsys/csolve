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
module SM = M.StringMap
module I  = Index
module Sl = Sloc
module RA = Refanno
module Ct = Ctypes
module FC = FixConstraint
module CM = CilMisc
module Hf = Heapfun
module ST = Ssa_transform
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
open Shape

let acp = Array.copy
let hcp = Hashtbl.copy

type unf  = Sl.t list list
type app  =
  | App of Sl.t * Ct.hf_appl * unf
  | Cnc of Sl.t * string

let concretizes l = function
  | Cnc (l', vn) -> Sloc.eq l l'
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

let var_of_lv = function
  | (Var v, _) -> v
  | lv -> E.error "var_of_lv"; assertf "var_of_lv"
  
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
    |> (function App (cl, _, _) -> Some cl 
               | Cnc (cl, _)    -> Some cl)
  with Not_found -> None 

let is_unf_by appm al cl =
  is_unf appm al
  |> function
     | Some cl' -> Sl.eq cl cl'
     | None -> false

let mk_fold_cnc al cl = RA.Gen (cl, al)

let mk_fold_hf (hf, ls, _) unf = RA.HGen (hf, ls, unf) 

let mk_fold appm al =
  match SLM.find al appm with
    | Cnc (cl, _) -> mk_fold_cnc cl al
    | App (_, appl, unf) -> mk_fold_hf appl unf

let mk_unfold_cnc vn cl al = RA.Ins (vn, cl, al)

let mk_unfold_hf al intrs = RA.HIns (al, intrs)

let mk_unfold al appm =
  match SLM.find al appm with
    | Cnc (cl, vn) -> mk_unfold_cnc vn cl al 
    | App (_, appl, unf) -> mk_unfold_hf al unf

let generalize_cnc sto al cl =
  ([mk_fold_cnc al cl], CtIS.remove sto cl)

let generalize_hf sto cl ((hf, ls, _) as app) ins env =
  let _      = asserts (cl = (List.hd ins |> List.hd)) "generalize_hf" in
  let _, sto = Hf.gen_shp app ins SlSS.empty sto env in
  ([mk_fold_hf app ins], sto)

let generalize sto gst ctm me appm al =
  let _   = assert (SLM.mem al appm) in 
  let env = Hf.test_env in
  let (ann, sto) =
    match SLM.find al appm with
  | App (cl, app, ins)  -> generalize_hf sto cl app ins env
  | Cnc (cl, _)         -> generalize_cnc sto al cl in
  (ann, sto, gst, me, SLM.remove al appm)
 
let instantiate_cnc sto me appm v al =
  let cl   = always_get_cl me al in
  let cl   = Sl.copy_concrete al in
  let _    = set_cl me v cl in
  let sto' = CtIS.find sto al |> CtIS.add sto cl in
  let vn   = v.vname in
  ([mk_unfold_cnc vn al cl], sto', SLM.add al (Cnc (cl, vn)) appm)

let instantiate_hf sto appm me env v al cl =
  let (hf, _, _) as app =
    CtIS.hfuns sto |> Ct.hf_appl_binding_of al |> M.maybe in
  let ins    = Hf.fresh_unfs_of_hf cl hf env in
  let _, sto = Hf.ins_shp al [al] ins SlSS.empty sto env in
  let _      = set_cl me v cl in
  let appm   = SLM.add al (App(cl, app, ins)) appm in
  ([mk_unfold_hf al ins], sto, appm)

let instantiate_aux sto gst ctm me appm v al cl = 
  let hf_env = Hf.test_env in
  begin if CtIS.hfuns sto |> List.exists (Ct.hf_appl_binds al) then
    instantiate_hf sto appm me hf_env v al cl
  else
     let _ = if not (CtIS.mem sto al) then P.printf "%a in %a@!" Sl.d_sloc al
                CtIS.d_store sto |> ignore in
     let _ = asserts (CtIS.concrete_part sto
                   |> M.flip CtIS.mem al) "instantiate_aux" in
     instantiate_cnc sto me appm v al end
  |> (fun (ann, sto, appm) -> (ann, sto, gst, me, appm))

let instantiate sto gst ctm me appm v al cl =
  let _ = asserts (Sl.is_abstract al) "instantiate" in
  let _ = asserts (Sl.is_concrete cl) "instantiate" in
  (*let _ = P.printf "@[INST_STORE(%a): %a@]@!@!" Sl.d_sloc cl CtIS.d_store sto
   * in*)
  if is_unf_by appm al cl then
    ([], sto, gst, me, appm)
  else if SLM.mem al appm then
    let (ann, sto,gst,me,appm) = generalize sto gst ctm me appm al in
    let (ann',sto,gst,me,appm) = instantiate_aux sto gst ctm me appm v al cl in
      (ann ++ ann', sto, gst, me, appm)
  else
    instantiate_aux sto gst ctm me appm v al cl

let is_deref = function
  | Lval (Mem _, _) -> true
  | _ -> false

let ctm_sloc_of_expr ctm e =
  try
    match CtI.ExpMap.find e ctm with
    | Ct.Ref (s, _) -> Some s
    | _             -> None
  with Not_found ->
    Pretty.printf "Could not find %a\n\n" d_exp e; assert false

let ctm_ct_of_expr ctm e =
  try
    CtI.ExpMap.find e ctm
  with Not_found ->
    Pretty.printf "Could not find %a\n\n" d_exp e; assert false

let ref_v_of_expr ctm e =
  let rec v_rec = function
    | Lval (Var v, _) as e when isPointerType v.vtype ->
        Some (v, ctm_sloc_of_expr ctm e |> M.maybe)
    | BinOp (_, e1, e2, _) -> begin
        match (v_rec e1, v_rec e2) with
        | (None, None) -> None
        | (None, vl) | (vl, None)  -> vl
        | (Some (v, l) as vl, Some (_, l')) when Sl.eq l l' -> vl
        | _ -> assert false end
    | CastE (_, e) -> v_rec e
    | _ -> None in
  v_rec e

let al_of_expr ctm me e =
  ref_v_of_expr ctm e
  |>> fun (v, ctal) ->
      match get_al me v with
      | Some al -> Some al 
      | None    -> Some (ctal >> set_al me v)

let cl_of_expr ctm me e =
  ref_v_of_expr ctm e
  |>> (fun x -> fst x |> get_cl me)

let possibly_fresh_cl_of_expr ctm me e =
  let v = ref_v_of_expr ctm e |> M.maybe |> fst in
  get_al me v |> M.maybe |> fun x -> always_get_cl me x v

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
  match cl_of_expr ctm me em with
    | Some l -> 
        let i = ind_of_expr ctm em |> M.maybe in
        begin match CtIS.find sto l |> CtIL.find i with
        | [(_, fld)] -> CtIF.sloc_of fld
        | _          -> assert false end
    | None  -> assert false

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
  let cl = possibly_fresh_cl_of_expr ctm me e in
    instantiate sto gst ctm me appm v al cl

let annotate_write sto gst ctm me appm e ct =
  let v     = CM.referenced_var_of_exp e in
  let al    = al_of_expr ctm me e |> M.maybe in
  let cl    = possibly_fresh_cl_of_expr ctm me e in
  let il    = ind_of_expr ctm e in
    instantiate sto gst ctm me appm v al cl
  (* check subtyping of ct vs sto' |> find |> cl |> find |> il *)


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
  | (Var v, _), e when expr_has_ptr_type ctm e ->
    let e = e >> (CilMisc.is_pure_expr CilMisc.StringsArePure <+>
      (fun b -> asserts b "impure expr")) in
    let _ = al_of_expr ctm me e |> maybe_set_al me v in
    let _ = cl_of_expr ctm me e     |> maybe_set_cl me v in
    ([], sto, gst, me, appm)

  (* *v1 := *v2 shouldn't be possible *)
  | (Mem e1, _), Lval (Mem e2, _) ->
      E.error "annotate_set: lv = *%a, e=*%a" Cil.d_exp e1 Cil.d_exp e2;
      assertf "annotate_set: illegal looking set?"

  (* *v := e *)
  | (Mem e1, _), e ->
      let ct = ctm_ct_of_expr ctm e in 
        annotate_write sto gst ctm me appm e1 ct

  (* v := e | e is constant *)
  | (Var v, _), _ ->
    ([], sto, gst, me, appm)

  | lv, e ->
      E.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let conc_malloc me x y v =
  let _ = assert (Sl.is_abstract x) in
  let _ = assert (Sl.is_abstract y) in
  match get_al me v with
  | Some al -> always_get_cl me al v
  | None    -> set_cl me v y; always_get_cl me y v

let conc_lv ctm me = function 
  | (Var v, _) as lv ->
      get_al me v
      |>> (fun al -> Some (always_get_cl me al v))
  | lv ->
      Errormsg.error "sloc_of_ret";
      assertf "sloc_of_ret"

let gen_ann_if_new ctm gst me (sto, appm) = function
  | RA.New (x, y) as nan ->
    let _= asserts (Sl.is_abstract y) "concretize_new" in
    if Sl.is_abstract x then
      if is_unf appm y |> M.maybe_bool then
        let (ann, sto, gst, me, appm) =
          generalize sto gst ctm me appm y in
        (sto, appm), ann ++ [nan]
      else
        (sto, appm), [nan] 
    else 
        assertf "conc_ann_if_new"
  | ann           -> (sto, appm), [ann]

let do_sto_anno me env sto = function
  | RA.Gen  (cl, al)       -> CtIS.remove sto cl
  | RA.HGen (hf, sls, ins) -> Hf.fold_hf_shapes_on_sto (hf, sls, []) ins sto env
  | RA.Ins  _    
  | RA.HIns _ 
  | _ -> assertf "do_sto_anno"

let fold_if_open (res, appm) = function
    | RA.Ins  (_, a, _)
    | RA.HIns (a, _) ->
        if SLM.mem a appm then 
          mk_fold appm a :: res, SLM.remove a appm
        else
          res, appm
    | _              -> res, appm

let fold_all_open_locs ans appm =
     List.rev ans
  |> List.fold_left fold_if_open ([], appm)
  |> fst

let actually_fold_all_open_locs me env ans sto appm =
     fold_all_open_locs ans appm
  |> fun ans -> ans, List.fold_left (do_sto_anno me env) sto ans

let annotate_instr spec env ans sto gst ctm me appm = function 
  | Set (lv, e, _) -> annotate_set sto gst ctm me appm (lv, e)

  | Call (_, Lval ((Var fv), NoOffset), _,_) 
    when CilMisc.is_pure_function fv.vname ->
      ([], sto, gst, me, appm) 

  | Call (_, Lval ((Var fv), _), _, _) when fv.vname = "csolve_fold_all" ->
      E.error "csolve_fold_all unsupported"; assert false

      (* Shape inference only creates an aloc for this malloc. we need to create
       * a cloc*)
      (* i may actually need to replace the New annot with the NewC annot... *)
  | Call (rv, Lval ((Var fv), NoOffset), _, _) when fv.vname = "malloc" ->
      let v = (rv |> M.maybe) |> var_of_lv in (* this might not work *)
      let newC, (al, cl) = begin match ans with 
        | [RA.New (x, y)] ->
            let cl = conc_malloc me x y v in
            [RA.NewC (x, y, cl)], (y, cl)
        | _            -> 
          E.error "annotate_malloc"; assertf "annotate_malloc" end in
      let (anns, sto, gst, me, appm) = instantiate sto gst ctm me appm v al cl in
      (newC ++ anns, sto, gst, me, appm)

  | Call (rv, Lval ((Var fv), NoOffset), _, _) ->
      let ca_sto = CtISp.funspec spec 
                |> SM.find fv.vname |> fst
                |> (fun x -> x.Ct.sto_out) in
      let anns, sto = actually_fold_all_open_locs me env ans sto appm in
      let sto       = CtIS.upd sto ca_sto in
      let (sto, appm), anns = M.mapfold (gen_ann_if_new ctm gst me) (sto, appm) ans in
      (*let conc, anns' = Misc.mapfold generalize conc globalslocs in*)
      let anns = List.concat anns in
      let _ = rv |>> conc_lv ctm me |> ignore in
      (anns, sto, gst, me, appm) 

  | i -> E.s <| bug "Unimplemented constrain_instr: %a@!@!" dn_instr i

let upd_wld (annos, _, _, _, _) (annos', sto, gst, me, appm) =
  (annos' :: annos, sto, gst, me, appm)

let annotate_block spec anns sto gst ctm me is =
  let init = ([], sto, gst, me, SLM.empty) in
  let env  = Heapfun.test_env in
  List.fold_left2 begin fun ((_, sto, gst, me, appm) as wld) ans ins ->
    annotate_instr spec env ans sto gst ctm me appm ins
    |> upd_wld wld end init anns is
  |> M.app_fst5 List.rev
  |> begin fun (ans, _, _, _, appm) ->
       fold_all_open_locs (List.concat ans) appm
    |> (fun x -> M.append_to_last x ans) end

let annot_iter spec cfg sto ctm me anna =
  let nblocks    = Array.length cfg.Ssa.blocks in
  let do_block j =
    match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
    | Instr is ->
        annotate_block spec anna.(j) sto CtIS.empty ctm me is
        |> M.m2append anna.(j)
        |> Array.set anna j
    | _        -> () in
  M.range 0 nblocks
  |> List.iter do_block

let build_join_store_of_blocks sto anna = sto

let nil_cnca_of_sto sto =
  CtIS.domain sto
  |> List.fold_left (fun c x -> SLM.add x SLM.empty c) SLM.empty

let annotate_cfg spec cfg shp =
  let nblocks        = Array.length cfg.Ssa.blocks in
  let anna, sto, ctm = acp shp.anna, shp.store, shp.etypm in
  let me             = (Hashtbl.create 16, Hashtbl.create 16) in
  let _              = annot_iter spec cfg sto ctm me anna in
  let sto            = build_join_store_of_blocks sto anna in
  { vtyps = shp.vtyps;
    etypm = shp.etypm;
    store = sto;
    anna  = anna; }

let annotate_shpm scim spec shpm =
  SM.fold begin fun fn shp shpm ->
    let anno =
         SM.find fn scim
      |> (fun x -> annotate_cfg spec x.ST.cfg) in
    SM.add fn (anno shp) shpm end shpm SM.empty
