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
module C  = Constraint
module W  = Wrapper
module CI = CilInterface

open Misc.Ops
open Cil

type t = {
  sci  : ST.ssaCfgInfo;
  ws   : C.wf list;
  cs   : C.t list;
  envm : W.cilenv IM.t;
  gnv  : W.cilenv; 
}

let create env sci = 
  {sci  = sci;
   cs   = [];
   ws   = [];
   envm = IM.empty;
   gnv  = W.ce_unroll sci.ST.fdec.svar env |> fst}

let add_cons ws cs me =
  {sci  = me.sci; 
   cs   = cs ++ me.cs; 
   ws   = ws ++ me.ws; 
   envm = me.envm;
   gnv  = me.gnv}

let add_env i env me = 
  {me with envm = IM.add i env me.envm}

let get_cons me =
  (me.ws, me.cs)

let stmt_of_block me i =
  me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt

let location_of_block me i =
  Cil.get_stmtLoc (stmt_of_block me i).skind 

let phis_of_block me i = 
  me.sci.ST.phis.(i) 
  |> Misc.map fst

let outenv_of_block me i =
  IM.find i me.envm

let inenv_of_block me i = 
  if i = 0 then me.gnv else
    let (idom, _) = me.sci.ST.gdoms.(i) in
    outenv_of_block me idom

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
