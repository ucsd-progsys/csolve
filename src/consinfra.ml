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

module ST  = Ssa_transform
module IM  = Misc.IntMap
module SM  = Misc.StringMap
module C   = FixConstraint
module FI  = FixInterface 
module CI  = CilInterface
module EM  = Ctypes.ExpMap
module LI  = Inferctypes
module Sh  = Shape
module LM  = Sloc.SlocMap
module IIM = Misc.IntIntMap

open Misc.Ops
open Cil

type wld = FI.cilenv * FI.refstore * CilTag.t option 

type t = {
  tgr     : CilTag.o;
  sci     : ST.ssaCfgInfo;
  cf      : FI.alocmap;
  ws      : C.wf list;
  cs      : C.t list;
  ds      : C.dep list;
  wldm    : wld IM.t;
  gnv     : FI.cilenv; 
  formalm : unit SM.t;
  etm     : Ctypes.ctemap;
  ltm     : (varinfo * Ctypes.ctype) list;
  astore  : FI.refstore;
  anna    : Refanno.block_annotation array;
  ffmsa   : Sh.final_fields_annot array;
  cstoa   : (FI.refstore * Sloc.t list * Refanno.cncm) array; 
  ctab    : Refanno.ctab;
  undefm  : unit SM.t;
  edgem   : (Cil.varinfo * Cil.varinfo) list IIM.t;
  phibt   : (string, (FI.name * FI.refctype)) Hashtbl.t
}

let ctype_of_varinfo ctl v =
  try List.assoc v ctl with Not_found ->
    assertf "ctype_of_varinfo: unknown var %s" v.Cil.vname

let ctype_of_local locals v =
  try List.assoc v locals with 
    Not_found -> assertf "ctype_of_local: unknown var %s" v.Cil.vname

let strengthen_cloc = function
  | ct, None | (Ctypes.CTInt (_, _) as ct), _  -> ct
  | (Ctypes.CTRef (_, x)), Some cl      -> Ctypes.CTRef (cl, x) 

let strengthen_refs theta v (vn, cr) =
  let ct  = FI.ctype_of_refctype cr in
  let clo = Refanno.cloc_of_varinfo theta v in
  let ct' = strengthen_cloc (ct, clo) in
  let cr' = FI.t_ctype_refctype ct' cr in 
  (vn, cr')

let is_origcilvar v = 
  match ST.deconstruct_ssa_name v.vname with
  | None -> true
  | _    -> false

let env_of_fdec gnv fdec locals theta =
  let rft  = FI.ce_find_fn fdec.svar.vname gnv in
  let env0 = FI.args_of_refcfun rft 
             |> Misc.map2 (strengthen_refs theta) fdec.Cil.sformals 
             |> List.map (Misc.app_fst FI.name_of_string)
             |> FI.ce_adds gnv in
  fdec.slocals 
  |> List.filter is_origcilvar
  |> Misc.map (fun v -> (FI.name_of_varinfo v, FI.t_true (ctype_of_local locals v)))
  |> FI.ce_adds env0

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

let diff_binding conc (al, x) = 
  if LM.mem al conc then
    LM.find al conc |> eq_tagcloc x |> not
  else true

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

let cstoa_of_annots fname gdoms conca astore =
  let emp = FI.refstore_empty in
  Array.mapi begin fun i (conc,conc') ->
    let idom, _ = gdoms.(i) in 
    if idom < 0 then (emp, [], conc') else
      let _,idom_conc = conca.(idom) in
      let joins, ins  = Sloc.slm_bindings conc 
                        |> List.partition (diff_binding idom_conc) in
      let inclocs     = List.map (snd <+> fst) ins in
      let sto         = joins 
                        |> List.fold_left begin fun sto (al, (cl, _)) -> 
                             FI.refstore_get astore al |> FI.refstore_set sto cl
                           end emp
                        |> FI.store_of_refstore 
                        |> FI.refstore_fresh fname in
      (sto, inclocs, conc')
  end conca

let edge_asgnm_of_phia phia =
  Misc.array_to_index_list phia
  |> List.fold_left begin fun em (j, asgns) -> 
       Misc.transpose asgns 
       |> List.fold_left begin fun em (i, vvis) -> 
            IIM.add (i,j) (vvis : (Cil.varinfo * Cil.varinfo) list) em 
          end em  
     end IIM.empty 

let create tgr gnv gst sci shp =
  let fdec    = sci.ST.fdec in
  let env     = env_of_fdec gnv fdec shp.Sh.vtyps shp.Sh.theta in
  let istore  = FI.ce_find_fn fdec.svar.vname gnv |> FI.stores_of_refcfun |> fst |> Ctypes.PreStore.upd gst in
  let lastore = FI.refstore_fresh fdec.svar.vname shp.Sh.store in
  let astore  = Ctypes.PreStore.upd gst lastore in
  let formalm = formalm_of_fdec sci.ST.fdec in
  let tag     = CilTag.make_t tgr fdec.svar.vdecl fdec.svar.vname 0 0 in 
  let loc     = fdec.svar.vdecl in
  let cf      = Refanno.aloc_of_cloc shp.Sh.theta in
  let cs, ds  = FI.make_cs_refstore cf env Ast.pTrue istore astore false None tag loc in 
  let cstoa   = cstoa_of_annots fdec.svar.vname sci.ST.gdoms shp.Sh.conca astore in
  {tgr     = tgr;
   sci     = sci;
   cf      = cf;
   ws      = FI.make_wfs_refstore cf env lastore lastore tag;
   cs      = cs;
   ds      = ds;
   wldm    = IM.empty;
   gnv     = env;
   formalm = formalm;
   etm     = shp.Sh.etypm;
   ltm     = shp.Sh.vtyps;
   astore  = astore;
   anna    = shp.Sh.anna;
   ffmsa   = shp.Sh.ffmsa;
   cstoa   = cstoa;
   ctab    = shp.Sh.theta;
   undefm  = make_undefm formalm sci.ST.phis;
   edgem   = edge_asgnm_of_phia sci.ST.phis;
   phibt   = Hashtbl.create 17
}

let add_cons (ws, cs, ds) me =
  {me with cs = cs ++ me.cs; ws = ws ++ me.ws; ds = ds ++ me.ds}

let add_wld i wld me = 
  {me with wldm = IM.add i wld me.wldm}

let get_cons me =
  (me.ws, me.cs, me.ds)

let get_astore me = 
  me.astore

let stmt_of_block me i =
  me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt

let annotstmt_of_block me i = 
  (me.anna.(i), me.ffmsa.(i), stmt_of_block me i)

let get_fname me = 
  me.sci.ST.fdec.svar.vname 

let get_alocmap me =
  me.cf

let location_of_block me i =
  Cil.get_stmtLoc (stmt_of_block me i).skind 

let tag_of_instr me block_id instr_id loc = 
  CilTag.make_t me.tgr loc (get_fname me) block_id instr_id

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
  i |> doms_of_block me.sci.ST.gdoms []
    |> Misc.map_partial (function (i,Some b) -> Some (i,b) | _ -> None)
    |> Misc.map (pred_of_block me.sci.ST.ifs)
    |> Ast.pAnd

let guard_of_block me i jo = 
  let p = entry_guard_of_block me i in
  match jo with None -> p | Some j -> 
    if not (Hashtbl.mem me.sci.ST.edoms (i, j)) then p else
      let b' = Hashtbl.find me.sci.ST.edoms (i, j) in 
      let p' = pred_of_block me.sci.ST.ifs (i, b') in
      Ast.pAnd [p; p']

let csto_of_block  = fun me i -> me.cstoa.(i) |> fst3
let succs_of_block = fun me i -> me.sci.ST.cfg.Ssa.successors.(i)

let asgns_of_edge me i j = 
  try IIM.find (i, j) me.edgem with Not_found -> []

let annots_of_edge me i j =
  let iconc' = me.cstoa.(i) |> thd3 in
  let jsto   = csto_of_block me j in
  LM.fold begin fun al (cl, t) acc -> 
    if FI.refstore_mem cl jsto then acc else 
      if Refanno.tag_dirty t then (Refanno.Gen (cl, al) :: acc) else
        (Refanno.WGen (cl, al) :: acc)
  end iconc' []  



  (*
let is_formal fdec v =
  fdec.sformals
  |> Misc.map (fun v -> v.vname)
  |> List.mem v.vname 
*)

let is_undefined me v = 
  is_undef_var me.formalm v || SM.mem v.vname me.undefm

let ctype_of_expr me e = 
  try EM.find e me.etm with Not_found ->
    let _ = Errormsg.error "ctype_of_expr: unknown expr = %a" Cil.d_exp e in
    assertf "Not_found in ctype_of_expr"

let ctype_of_varinfo me v =
  let ct  = ctype_of_varinfo me.ltm v in
  let clo = Refanno.cloc_of_varinfo me.ctab v in
  let rv  = strengthen_cloc (ct, clo) in
  (* let _   = Pretty.printf "ctype_of_varinfo v = %s, ct = %a \n" v.vname Ctypes.d_ctype ct in*)  
  rv

let refctype_of_global me v =
  FI.ce_find (FI.name_of_string v.Cil.vname) me.gnv

let phis_of_block me i = 
  me.sci.ST.phis.(i) 
  |> Misc.map fst

let outwld_of_block me i =
  IM.find i me.wldm

let bind_of_phi me v =
  Misc.do_memo me.phibt begin fun v -> 
    let vn = FI.name_of_varinfo v in
    let cr = ctype_of_varinfo me v |> FI.t_fresh in
    (vn, cr)
  end v v.vname

let idom_of_block = fun me i -> fst me.sci.ST.gdoms.(i)

let inenv_of_block me i =
  if idom_of_block me i < 0 then
    me.gnv
  else
    let env0  = idom_of_block me i |> outwld_of_block me |> fst3 in
    let phibs = phis_of_block me i |> List.map (bind_of_phi me) in
    FI.ce_adds env0 phibs

let inwld_of_block me = function
  | j when idom_of_block me j < 0 ->
      (me.gnv, me.astore, None)
  | j ->
      let _,sto,_      = idom_of_block me j |> outwld_of_block me in 
      let csto,incls,_ = me.cstoa.(j) in
      let loc          = location_of_block me j in
      let tag          = tag_of_instr me j 0 loc in
      (inenv_of_block me j, me.astore, Some tag)  
      (* Copy "inherited" conc-locations *)
      |> Misc.flip (List.fold_left begin fun (env, st, t) cl ->
          (env, (FI.refstore_get sto cl |> FI.refstore_set st cl), t)
         end) incls
      (* Add fresh bindings for "joined" conc-locations *)
      |> FI.refstore_fold begin fun cl ld wld ->
          fst <| FI.extend_world me.cf csto cl cl false loc tag wld
         end csto 

let is_reachable_block me i = 
  i = 0 || idom_of_block me i >= 0
 
