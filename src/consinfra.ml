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

module F  = Format
module ST = Ssa_transform
module IM = Misc.IntMap
module SM = Misc.StringMap
module C  = Constraint
module FI = FixInterface 
module CI = CilInterface
module EM = Inferctypes.ExpMap


open Misc.Ops
open Cil

type wld = FI.cilenv * FI.refstore

type t = {
  sci     : ST.ssaCfgInfo;
  ws      : C.wf list;
  cs      : C.t list;
  wldm    : wld IM.t;
  gnv     : FI.cilenv; 
  formalm : unit SM.t;
  ctm     : Inferctypes.ctemap;
  astore  : FI.refstore;
  anna    : Refanno.block_annotation array;
  ctab    : Refanno.ctab
}

let ctype_of_varinfo ctm v = 
  let e = Cil.Lval (Cil.Var v, Cil.NoOffset) in
  try EM.find e ctm with Not_found -> 
    assertf "ctype_of_varinfo: unknown var %s" v.Cil.vname

let ctype_of_local locals v =
  try List.assoc v locals with 
    Not_found -> assertf "ctype_of_local: unknown var %s" v.Cil.vname

let env_of_fdec gnv fdec locals =
  let (args, _) = FI.ce_find_fn (FI.name_of_varinfo fdec.svar) gnv in
  let env0 = FI.ce_adds gnv args in
    fdec.slocals 
  |> List.filter ST.is_origcilvar
  |> Misc.map (fun v -> (FI.name_of_varinfo v, FI.t_true (ctype_of_local locals v)))
  |> FI.ce_adds env0

let formalm_of_fdec fdec = 
  List.fold_left (fun sm v -> SM.add v.vname () sm) SM.empty fdec.Cil.sformals

let create gnv sci (locals, ctm, store) (anna, ctab) =
  let fdec   = sci.ST.fdec in
  let env    = env_of_fdec gnv fdec locals in
  let astore = FI.refstore_fresh store in 
  {sci     = sci;
   cs      = [];
   ws      = FI.make_wfs_refstore env astore fdec.svar.vdecl;
   wldm    = IM.empty;
   gnv     = env;
   formalm = formalm_of_fdec sci.ST.fdec;
   ctm     = ctm;
   astore  = astore;
   anna    = anna;
   ctab    = ctab}

let add_cons ws cs me =
  {{me with cs = cs ++ me.cs} with ws = ws ++ me.ws}

let add_wld i wld me = 
  {me with wldm = IM.add i wld me.wldm}

let get_cons me =
  (me.ws, me.cs)

let get_astore me = 
  me.astore

let stmt_of_block me i =
  me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt

let annotstmt_of_block me i = 
  (me.anna.(i), stmt_of_block me i)

let location_of_block me i =
  Cil.get_stmtLoc (stmt_of_block me i).skind 

let phis_of_block me i = 
  me.sci.ST.phis.(i) 
  |> Misc.map fst

let outwld_of_block me i =
  IM.find i me.wldm

let inwld_of_block me = function
  | 0 -> 
      (me.gnv, FI.refstore_empty)
  | i ->
    let (idom, _) = me.sci.ST.gdoms.(i) in
    let (env,_)   = outwld_of_block me idom in
    (env, FI.refstore_empty)


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
      let p = CI.pred_of_cilexp e in
      if b then p else (Ast.pNot p)

let guard_of_block me i = 
  i |> doms_of_block me.sci.ST.gdoms []
    |> Misc.map_partial (function (i,Some b) -> Some (i,b) | _ -> None)
    |> Misc.map (pred_of_block me.sci.ST.ifs)
    |> Ast.pAnd

let get_fname me = 
  FI.name_of_varinfo me.sci.ST.fdec.svar 

let is_formal fdec v =
  fdec.sformals
  |> Misc.map (fun v -> v.vname)
  |> List.mem v.vname 

let is_undefined me v =
  ST.is_origcilvar v && not (SM.mem v.vname me.formalm)

let ctype_of_expr me e = 
  EM.find e me.ctm

let ctype_of_varinfo me v =
  match ctype_of_varinfo me.ctm v with
  | Ctypes.CTInt (_, _) as ct -> 
      ct
  | Ctypes.CTRef (aloc, x)     -> 
      try 
        let cloc = Refanno.cloc_of_varinfo me.ctab v in 
        Ctypes.CTRef (cloc, x) 
      with Not_found -> assertf "cloc_of_varinfo fails on: %s" v.vname 
     

