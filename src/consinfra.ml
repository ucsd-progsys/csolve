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

module ST = Ssa_transform
module IM = Misc.IntMap
module SM = Misc.StringMap
module C  = FixConstraint
module FI = FixInterface 
module CI = CilInterface
module EM = Ctypes.ExpMap
module LI = Inferctypes
module LM = Sloc.SlocMap

open Misc.Ops
open Cil

type wld = FI.cilenv * FI.refstore * CilTag.t option 

type t = {
  tgr     : CilTag.o;
  sci     : ST.ssaCfgInfo;
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
  cstoa   : FI.refstore array; 
  ctab    : Refanno.ctab;
  undefm  : unit SM.t
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

let env_of_fdec gnv fdec locals theta =
  let rft  = FI.ce_find_fn fdec.svar.vname gnv in
  let env0 = FI.args_of_refcfun rft 
             |> Misc.map2 (strengthen_refs theta) fdec.Cil.sformals 
             |> List.map (Misc.app_fst FI.name_of_string)
             |> FI.ce_adds gnv in
  fdec.slocals 
  |> List.filter ST.is_origcilvar
  |> Misc.map (fun v -> (FI.name_of_varinfo v, FI.t_true (ctype_of_local locals v)))
  |> FI.ce_adds env0

let formalm_of_fdec fdec = 
  List.fold_left (fun sm v -> SM.add v.vname () sm) SM.empty fdec.Cil.sformals

let is_undef_var formalm v = 
  ST.is_origcilvar v && not (SM.mem v.vname formalm)

let make_undefm formalm phia =
  Array.to_list phia
  |> Misc.flatten
  |> List.filter (fun (_,vjs) -> vjs |> List.map snd |> List.exists (is_undef_var formalm))
  |> List.map fst
  |> List.fold_left (fun um v -> SM.add v.vname () um) SM.empty

let cstoa_of_annots fname cfg anna edgem astore =
  let conca = fst <| Refanno.reconstruct_conca cfg anna edgem in
  Array.map begin fun (conc: Sloc.t LM.t) ->
    FI.refstore_empty
    |> LM.fold (fun al cl sto -> FI.refstore_get astore al |> FI.refstore_set sto cl) conc
    |> FI.store_of_refstore 
    |> FI.refstore_fresh fname
  end conca

let create tgr gnv gst sci shp =
  let fdec    = sci.ST.fdec in
  let env     = env_of_fdec gnv fdec shp.LI.vtyps shp.LI.theta in
  let istore  = FI.ce_find_fn fdec.svar.vname gnv |> FI.stores_of_refcfun |> fst |> Ctypes.prestore_upd gst in
  let lastore = FI.refstore_fresh fdec.svar.vname shp.LI.store in
  let astore  = Ctypes.prestore_upd gst lastore in
  let formalm = formalm_of_fdec sci.ST.fdec in
  let tag     = CilTag.make_t tgr fdec.svar.vdecl fdec.svar.vname 0 0 in 
  let loc     = fdec.svar.vdecl in
  let cs, ds  = FI.make_cs_refstore env Ast.pTrue istore astore false None tag loc in 
  let cstoa   = cstoa_of_annots fdec.svar.vname sci.ST.cfg shp.LI.anna shp.LI.edgem astore in
  {tgr     = tgr;
   sci     = sci;
   ws      = FI.make_wfs_refstore env lastore tag;
   cs      = cs;
   ds      = ds;
   wldm    = IM.empty;
   gnv     = env;
   formalm = formalm;
   etm     = shp.LI.etypm;
   ltm     = shp.LI.vtyps;
   astore  = astore;
   anna    = shp.LI.anna;
   cstoa   = cstoa;
   ctab    = shp.LI.theta;
   undefm  = make_undefm formalm sci.ST.phis
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
  (me.anna.(i), stmt_of_block me i)

let get_fname me = 
  me.sci.ST.fdec.svar.vname 

let location_of_block me i =
  Cil.get_stmtLoc (stmt_of_block me i).skind 

let tag_of_instr me block_id instr_id loc = 
  CilTag.make_t me.tgr (* (location_of_block me block_id) *) loc (get_fname me) block_id instr_id

let phis_of_block me i = 
  me.sci.ST.phis.(i) 
  |> Misc.map fst

let outwld_of_block me i =
  IM.find i me.wldm

let inwld_of_block me = function
  | 0 -> 
      (me.gnv, me.astore, None)
  | i ->
      let (idom, _) = me.sci.ST.gdoms.(i) in
      let (env,_,t) = outwld_of_block me idom in
      (env, me.astore, t) 

let rec doms_of_block gdoms acc i =
  if i <= 0 then acc else
    let (idom,_) as x = gdoms.(i) in 
    let _ = asserts (idom < i) "doms_of_block" in
    doms_of_block gdoms (x::acc) idom 

let pred_of_block ifs (i,b) =
  match ifs.(i) with 
  | None         -> 
      assertf "pred_of_block"
  | Some (e,_,_) -> 
      let p = CI.pred_of_cilexp FI.skolem e in
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
