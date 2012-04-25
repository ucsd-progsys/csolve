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

(*******************************************************************)
(************* Constraint Generation Infrastructure ****************)
(*******************************************************************)

module Misc = FixMisc 
module Cs  = Constants
module ST  = Ssa_transform
module IM  = Misc.IntMap
module SM  = Misc.StringMap
module C   = FixConstraint
module FI  = FixInterface 
module FA  = FixAstInterface 
module CI  = CilInterface
module EM  = Ctypes.I.ExpMap
module Ix  = Index
module LI  = Inferctypes
module Sh  = Shape
module LM  = Sloc.SlocMap
module IIM = Misc.IntIntMap
module CM  = CilMisc
module YM  = Ast.Symbol.SMap
module Ct  = Ctypes
module ES  = Ct.EffectSet

open Misc.Ops
open Cil

type wld = FI.cilenv * Ct.refstore * CilTag.t option 

type t_sh = {
  astore  : Ct.refstore;
  aeffs   : ES.t;
  cstoa   : (Ct.refstore * Sloc.t list * Refanno.cncm) array; 
  shp     : Shape.t;
}

type t    = {
  tgr     : CilTag.o;
  sci     : ST.t;
  ws      : C.wf list;                                   (* wf constrs *)
  cs      : C.t list;                                    (* sub constrs *)
  ds      : C.dep list;
  wldm    : wld IM.t;
  gnv     : FI.cilenv;                                   (* ctype environment *)
  global_env : FI.cilenv;
  formalm : unit SM.t;
  undefm  : unit SM.t;
  edgem   : (Cil.varinfo * Cil.varinfo) list IIM.t;
  phim    : Ct.refctype SM.t;
  effsm   : ES.t IM.t;
  shapeo  : t_sh option;
  des     : (Cil.varinfo * Ct.refctype) list;
  (* phibt   : (string, (FI.name * FI.refctype)) Hashtbl.t; *)
  (* bindm   : (FI.cilenv * Ast.pred * FI.refctype) YM.t *)
}
    
let globalenv_of_t me = me.global_env

let ctype_of_local locals v =
  try List.assoc v locals with 
    Not_found -> assertf "ctype_of_local: unknown var %s" v.Cil.vname

let strengthen_cloc = function
  | ct, None 
  | (Ctypes.TVar _ as ct) , _
  | (Ctypes.Int (_, _) as ct), _ 
  | (Ctypes.Any _ as ct), _ 
  | (Ctypes.ARef as ct), _ -> ct
  | (Ctypes.Ref (_, x)), Some cl -> Ctypes.Ref (cl, x)

let strengthen_refs theta v (vn, cr) =
  let ct  = Ct.ctype_of_refctype cr in
  let clo = Refanno.cloc_of_varinfo theta v in
  let ct' = strengthen_cloc (ct, clo) in
  let cr' = FI.t_ctype_refctype ct' cr in 
  (vn, cr')

let is_origcilvar v = 
  match ST.deconstruct_ssa_name v.vname with
  | None -> true
  | _    -> false

let scalarenv_of_fdec gnv fdec =
  let args = FI.ce_find_fn fdec.svar.vname gnv
    (*         >> (Pretty.printf "scalarenv_of_fdec %s : @[%a@]\n" fdec.svar.vname Ct.d_refcfun)
*)           |>  Ct.args_of_refcfun
	         |>: (Misc.app_snd (fun c -> match c with
				   | Ct.Ref (_,(_,reft)) ->
                      Ct.Ref (Sloc.none, (Ix.top, reft))
                   | Ct.Int (w, (_, reft)) ->
                      Ct.Int (w, (Ix.top, reft))
                   | _ -> c))
             |>: (FA.name_of_string <**> FI.t_scalar_refctype) 
  in
  let locs = fdec.slocals
             |> List.filter is_origcilvar 
             |> Misc.map (FA.name_of_varinfo <*> (fun v -> FI.t_true (Ctypes.vtype_to_ctype v.Cil.vtype)))
  in
  args ++ locs
(*  >> List.iter (fun (n,rct) -> ignore <| Pretty.printf "scalarenv_of_fdec: %s := %a \n" (Ast.Symbol.to_string n) Ct.d_refctype rct)
*)  |> FI.ce_adds gnv

let env_of_fdec shp gnv fdec =
  let args = FI.ce_find_fn fdec.svar.vname gnv
             |> Ct.args_of_refcfun 
             |> Misc.map2 (strengthen_refs shp.Sh.theta) fdec.sformals
             |> List.map (Misc.app_fst FA.name_of_string)
  in
  let locs = fdec.slocals 
             |> List.filter is_origcilvar 
             |> Misc.map (FA.name_of_varinfo <*> 
                         (FI.t_true <.> ctype_of_local shp.Sh.vtyps)) in
  args ++ locs
  |> FI.ce_adds gnv

let env_of_fdec = function
  | None     -> scalarenv_of_fdec
  | Some shp -> env_of_fdec shp

(*
let env_of_fdec gnv fdec sho = 
  let strf, typf = match sho with
    | None     -> (id, fun _ -> Ctypes.scalar_ctype)
    | Some shp -> (Misc.map2 (strengthen_refs shp.Sh.theta) fdec.sformals, ctype_of_local shp.Sh.vtyps) in
  let env0 = 
    FI.ce_find_fn fdec.svar.vname gnv 
    |> FI.args_of_refcfun 
    |> strf 
    |> List.map (Misc.app_fst FI.name_of_string)
    |> FI.ce_adds gnv in
  fdec.slocals 
  |> List.filter is_origcilvar 
  |> Misc.map (FI.name_of_varinfo <*> (FI.t_true <.> typf))
  |> FI.ce_adds env0 

*)

let formalm_of_fdec fdec = 
  List.fold_left (fun sm v -> SM.add v.vname () sm) SM.empty fdec.Cil.sformals

let is_undef_var formalm v = 
  is_origcilvar v && not (SM.mem v.vname formalm)

let make_undefm formalm phia =
  Array.to_list phia
  |> Misc.flatten
  |> List.filter (fun (_,vjs) -> vjs |> List.map snd |> List.exists (is_undef_var formalm))
  |> List.map fst
  |> List.fold_left (fun um v -> SM.add v.vname () um) SM.empty

let eq_tagcloc (cl,t) (cl',t') = 
   Sloc.eq cl cl' && Refanno.tag_eq t t'

(*
let add_binding x env grd r me =
  let n = FI.name_of_varinfo x in
  if YM.mem n me.bindm then me else 
    {me with bindm = YM.add n (env, grd, r)} 
*)

(*
let canon_of_annot = function 
  | Refanno.WGen (cl, al) 
  | Refanno.Gen  (cl, al) 
  | Refanno.Ins  (al, cl) 
  | Refanno.NewC (_, al, cl) -> Some (cl, al)
  | _                        -> None

let alocmap_of_anna a = 
  a |> Array.to_list 
    |> Misc.flatten
    |> Misc.flatten
    |> Misc.map_partial canon_of_annot
    >> List.iter (fun (cl, al) -> ignore <| Pretty.printf "canon: %a -> %a \n" Sloc.d_sloc cl Sloc.d_sloc al)
    |> List.fold_left (fun cf (cl, al) -> AM.add cl al cf) AM.id

*)

let partition_diff_bindings cfrom cto =
  LM.fold begin fun al ctabto (diff, same) ->
    let ctabfrom = try LM.find al cfrom with Not_found -> LM.empty in
      LM.fold begin fun cl t (diff, same) ->
        try
          if Refanno.tag_eq t (LM.find cl ctabfrom) then
            (diff, (al, cl) :: same)
          else
            ((al, cl) :: diff, same)
        with Not_found ->
          ((al, cl) :: diff, same)
      end ctabto (diff, same)
  end cto ([], [])

let cstoa_of_annots fname gdoms conca astore =
  let emp = Ct.RefCTypes.Store.empty in
  Array.mapi begin fun i (conc,conc') ->
    let idom, _ = gdoms.(i) in
    if idom < 0 then (emp, [], conc') else
      let _,idom_conc = conca.(idom) in
      let joins, ins  = partition_diff_bindings idom_conc conc in
      let inclocs     = List.map snd ins in
      let sto         = joins 
                        |> List.fold_left begin fun sto (al, cl) ->
                             Ct.refstore_get astore al |> Ct.refstore_set sto cl
                           end emp
                        |> Ct.store_of_refstore 
                        |> FI.refstore_fresh fname 
      in (sto, inclocs, conc')
  end conca

let edge_asgnm_of_phia phia =
  Misc.array_to_index_list phia
  |> List.fold_left begin fun em (j, asgns) -> 
       Misc.transpose asgns 
       |> List.fold_left begin fun em (i, vvis) -> 
            IIM.add (i,j) (vvis : (Cil.varinfo * Cil.varinfo) list) em 
          end em  
     end IIM.empty 

let add_cons (ws, cs, des, ds) me =
  {me with cs = cs ++ me.cs; ws = ws ++ me.ws; ds = ds ++ me.ds; des = des ++ me.des}

let add_wld i wld me = 
  {me with wldm = IM.add i wld me.wldm}

let get_cons me = me.ws, me.cs, me.des, me.ds

let get_shapeo_astore = function Some x -> x.astore | _ -> Ct.RefCTypes.Store.empty

let get_astore me = get_shapeo_astore me.shapeo

let get_aeffs me = match me.shapeo with
  | None    -> ES.empty
  | Some sh -> sh.aeffs

let stmt_of_block me i =
  me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt

let get_fname me = 
  me.sci.ST.fdec.svar.vname 

let length_of_stmt stmt = match stmt.skind with 
  | Instr is -> List.length is
  | Return _ -> 0 
  | _        -> (if !Cs.safe then Errormsg.error "unknown stmt: %a" d_stmt stmt); 0 

let annotstmt_of_block me i = 
  match me.shapeo with 
  | Some {shp = shp} ->  
      (shp.Sh.anna.(i), shp.Sh.ffmsa.(i), stmt_of_block me i)
(*  | None     -> 
      let stmt = stmt_of_block me i in
      ((stmt |> length_of_stmt |> Misc.clone []), [], stmt)
*)

let location_of_block me i =
  Cil.get_stmtLoc (stmt_of_block me i).skind 

let tag_of_instr me block_id instr_id loc cause = 
  CilTag.make_t me.tgr loc (get_fname me) block_id instr_id cause

let rec doms_of_block gdoms acc i =
  if i <= 0 then acc else
    let (idom,_) as x = gdoms.(i) in 
    doms_of_block gdoms (x::acc) idom 

let pred_of_block ifs (i,b) =
  match ifs.(i) with 
  | None         -> 
      assertf "pred_of_block"
  | Some (e,_,_) -> 
      let p = CI.pred_of_cilexp e in
      if b then p else (Ast.pNot p)

let entry_guard_of_block me i = 
  i |> doms_of_block me.sci.ST.gdoms [] (* [(i, None)] *)
    |> Misc.map_partial (function (i,Some b) -> Some (i,b) | _ -> None)
    |> Misc.map (pred_of_block me.sci.ST.ifs)
    |> Ast.pAnd

let guard_of_block me i jo = 
  let p = entry_guard_of_block me i in
  match jo with None -> p | Some j -> 
    if not (Hashtbl.mem me.sci.ST.edoms (i, j)) then p else
      let b' = Hashtbl.find me.sci.ST.edoms (i, j) in 
      let p' = pred_of_block me.sci.ST.ifs (i, b') in
      (* let _  = Errormsg.log "guard_of_block edge i = %d j = %d p = %s \n" i j (Ast.Predicate.to_string p') in *)
      Ast.pAnd [p; p']

let succs_of_block = fun me i -> me.sci.ST.cfg.Ssa.successors.(i)
let csto_of_block  = fun {shapeo = Some shp} i -> shp.cstoa.(i) |> fst3 
let asgns_of_edge  = fun me i j -> try IIM.find (i, j) me.edgem with Not_found -> []

let annots_of_edge me i j =
  match me with 
  | {shapeo = Some shp} ->
      let iconc'         = shp.cstoa.(i) |> thd3 in
      let jsto, incls, _ = shp.cstoa.(j) in
      LM.fold begin fun al tagm acc ->
        LM.fold begin fun cl t acc ->
          if succs_of_block me j != [] &&
             (Ct.RefCTypes.Store.mem jsto cl || List.mem cl incls) then acc else
               if Refanno.tag_dirty t then (Refanno.Gen (cl, al) :: acc) else
                 (Refanno.WGen (cl, al) :: acc)
        end tagm acc
      end iconc' []  
  (* | _ -> [] *)

(*
let is_formal fdec v =
  fdec.sformals
  |> Misc.map (fun v -> v.vname)
  |> List.mem v.vname 
*)

let is_undefined me v = 
  is_undef_var me.formalm v || SM.mem v.vname me.undefm

let ctype_of_expr me e =
  match me.shapeo with 
  | Some {shp = shp} -> begin 
      try EM.find e shp.Sh.etypm with Not_found -> 
        let _ = Errormsg.error "ctype_of_expr: unknown expr = %a" Cil.d_exp e in
        assertf "Not_found in ctype_of_expr"
    end
  | _ -> assertf "ctype_of_expr" (* Ctypes.scalar_ctype *) 

let ctype_of_varinfo ctl v =
  try List.assoc v ctl with Not_found ->
    assertf "ctype_of_varinfo: unknown var %s" v.Cil.vname

let ctype_of_varinfo me v =
  match me.shapeo with 
  | Some {shp = shp} ->
      let ct = ctype_of_varinfo shp.Sh.vtyps v in
         strengthen_cloc (ct, Refanno.cloc_of_varinfo shp.Sh.theta v)
      (* >> Pretty.printf "ctype_of_varinfo v = %s, ct = %a \n" v.vname Ctypes.d_ctype ct *)
  | _ -> Ct.vtype_to_ctype v.Cil.vtype

 
let refctype_of_global me v =
  FI.ce_find (FA.name_of_string v.Cil.vname) me.gnv

let get_phis me =
  me.sci.ST.phis 
  |> Array.map (List.map fst) 
  |> Array.to_list 
  |> Misc.flatten

let bind_phis me =  
  me 
  |> get_phis
  |> List.map (fun v -> (v, v |> ctype_of_varinfo me |> FI.t_fresh))
  |> (fun vcrs -> { me with des = vcrs ++ me.des; phim = vcrs |>: Misc.app_fst (fun v -> v.vname) |> SM.of_list })

let phis_of_block me i = 
  me.sci.ST.phis.(i) 
  |> Misc.map fst
  (* >> List.iter (fun v -> ignore <| Pretty.printf "phis_of_block %d: %s \n" i v.Cil.vname) *)

let outwld_of_block me i =
  IM.find i me.wldm

(*
let bind_of_phi me v =
  Misc.do_memo me.phibt begin fun v -> 
    let vn = FI.name_of_varinfo v in
    let cr = ctype_of_varinfo me v |> FI.t_fresh in
    (vn, cr)
  end v v.vname
*)

let bind_of_phi me v = 
  try SM.find v.vname me.phim with Not_found ->
    assertf "bind_of_phi: unknown phi-var %s \n" v.vname

let idom_of_block = fun me i -> fst me.sci.ST.gdoms.(i)

let rec idom_parblock_of_block me i =
  let j = idom_of_block me i in
  let b = stmt_of_block me j in
    if CM.is_cobegin_block b || CM.is_foreach_block b then j else
      idom_parblock_of_block me j

let inenv_of_block me i =
  if idom_of_block me i < 0 then
    me.gnv
  else begin
    let env0  = idom_of_block me i |> outwld_of_block me |> fst3 in
    i |> phis_of_block me 
      |> List.map (FA.name_of_varinfo <*> bind_of_phi me) 
      |> FI.ce_adds env0 
  end

let extend_wld_with_clocs me j loc tag wld =
  match me with
  | {shapeo = Some shp} ->
      let _, sto, _    = idom_of_block me j |> outwld_of_block me in 
      let csto,incls,_ = shp.cstoa.(j) in
      wld
      (* Copy "inherited" conc-locations *)
      |> Misc.flip (List.fold_left begin fun (env, st, t) cl ->
          (env, (Ct.refstore_get sto cl |> Ct.refstore_set st cl), t)
         end) incls
      (* Add fresh bindings for "joined" conc-locations *)
      |> Misc.flip (Ct.RefCTypes.Store.fold_locs begin fun cl ld wld ->
          fst <| FI.extend_world csto cl cl false id loc tag wld
         end) csto
  | _ -> assertf "extend_wld_with_clocs: shapeo = None"

let fresh_abstract_effectset asto =
     asto
  |> Ct.RefCTypes.Store.domain
  |> List.fold_left begin fun effs l ->
       Ct.EffectSet.add effs l (FI.e_fresh l)
     end Ct.EffectSet.empty

let block_has_fresh_effects me i =
  CM.block_has_fresh_effects <| stmt_of_block me i

let make_effsm me =
  let aeffs  = get_aeffs me in
  let asto   = get_astore me in
  let blocks = me.sci.ST.cfg.Ssa.blocks in
  let t      = Hashtbl.create 17 in
  let rec block_effs i =
    if i < 0 then aeffs else
      let idom    = idom_of_block me i in
      let domeffs = Misc.do_memo t block_effs idom idom in
        if block_has_fresh_effects me i then
          fresh_abstract_effectset asto
        else domeffs
  in Misc.foldn
       (fun effsm i -> IM.add i (Misc.do_memo t block_effs i i) effsm)
       (Array.length blocks)
       IM.empty

let effectset_of_block me i =
  if i < 0 then get_aeffs me else IM.find i me.effsm

let inwld_of_block me = function
  | j when idom_of_block me j < 0 ->
      (me.gnv, get_astore me, None)
  | j ->
      let loc   = location_of_block me j in
      let msgo  = Some (Printf.sprintf "%s : entry" (get_fname me)) in
      let cause = CilTag.Raw "ConsInfra.inwld_of_block" in
      let tag   = tag_of_instr me j 0 loc cause in 
      (inenv_of_block me j, get_astore me, Some tag)
      |> ((me.shapeo <> None) <?> extend_wld_with_clocs me j loc tag)

let is_reachable_block me i = 
  i = 0 || idom_of_block me i >= 0

let has_shape = function 
  | {shapeo = Some _} -> true 
  | _                 -> false

(*
let definitions_of_block me i : (name * C.reft) list = 
  let env ,_,_ = inwld_of_block me i in
  let env',_,_ = outwld_of_block me i in
  FI.new_bindings env env'

let get_definitions me =
  IM.fold (fun i wld acc ->
    let env = env_of_wld wld in
    let grd = guard_of_block me i in
    i |> defs_of_block me
      |> FI.name_of_string
      |> List.map (Misc.app_snd (fun n -> get_reft wld n))
      |> (fun xs -> (env, grd, xs) :: acc)
  ) me.wldm 
*)

let create_shapeo tgr gnv env gst sci = function
  | None -> 
      ([], [], [], None)
  | Some shp ->
      let lastore = FI.refstore_fresh sci.ST.fdec.svar.vname shp.Sh.store in
      let astore  = Ct.RefCTypes.Store.upd gst lastore in
      let aeffs   = fresh_abstract_effectset astore in
      let cstoa   = cstoa_of_annots sci.ST.fdec.svar.vname sci.ST.gdoms shp.Sh.conca astore in
      let cause   = CilTag.Raw "ConsInfra.create_shapeo" in
      let fn      = sci.ST.fdec.svar.vname in
      let tag     = CilTag.make_t tgr sci.ST.fdec.svar.vdecl fn 0 0 cause in
      let loc     = sci.ST.fdec.svar.vdecl in
      let ws      = FI.make_wfs_refstore env lastore lastore ++ FI.make_wfs_effectset env lastore aeffs in
      let irf     = FI.ce_find_fn sci.ST.fdec.svar.vname gnv in
      let istore  = irf |> Ct.stores_of_refcfun |> fst in
      let cs1, ds1 = FI.make_cs_refstore env Ast.pTrue istore lastore false None tag loc in
      let cs2, ds2 = FI.make_cs_effectset env Ast.pTrue lastore istore aeffs irf.Ct.effects None tag loc in
      let cs3, ds3 = FI.make_cs_effectset env Ast.pTrue lastore lastore (ES.apply FI.t_false_refctype aeffs) aeffs None tag loc in
      ws, cs1 ++ cs2 ++ cs3, ds1 ++ ds2 ++ ds3, Some { astore  = astore; cstoa = cstoa; shp = shp; aeffs = aeffs }

let create tgr gnv gst sci sho = 
  let formalm = formalm_of_fdec sci.ST.fdec in
  let env     = env_of_fdec sho gnv sci.ST.fdec in
  let ws, cs, ds, sh_me = create_shapeo tgr gnv env gst sci sho in
  { tgr     = tgr
  ; sci     = sci
  ; ws      = ws
  ; cs      = cs
  ; ds      = ds
  ; des     = []
  ; wldm    = IM.empty
  ; gnv     = env
  ; global_env = gnv
  ; formalm = formalm
  ; undefm  = make_undefm formalm sci.ST.phis
  ; edgem   = edge_asgnm_of_phia sci.ST.phis
  ; phim    = SM.empty
  ; effsm   = IM.empty
  ; shapeo  = sh_me}
  |> bind_phis
  |> fun me -> {me with effsm = make_effsm me}
 
