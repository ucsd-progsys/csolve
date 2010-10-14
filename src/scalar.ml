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

module CM = CilMisc
module VM = CM.VarMap
module Sy = Ast.Symbol
module FI = FixInterface
module SM = Misc.StringMap
module ST = Ssa_transform
module Ix = Ctypes.Index
module Co = Constants
module Q  = Ast.Qualifier
module Ci = Consindex
module E  = Errormsg

open Misc.Ops

type scalar_const = Offset of int | UpperBound of int | Periodic of int * int

(***************************************************************************)
(************************* Scrape Scalar Qualifiers ************************)
(***************************************************************************)

let hash_of_ciltype t = 
  Pretty.dprintf "%a ### %a " Cil.d_typsig (Cil.typeSig t) Cil.d_attrlist (Cil.typeAttrs t)
  |> Pretty.sprint ~width:80

let type_decs_of_file (cil: Cil.file) : (Cil.location * Cil.typ) list =
  let x = ref [] in 
  CM.iterVars cil begin fun v -> match v.Cil.vtype with 
    | Cil.TFun (t,_,_,_) | t -> x := (v.Cil.vdecl, t) :: !x
  end; 
  !x 
  |> Misc.kgroupby (snd <+> hash_of_ciltype)
  |> Misc.map (function (_,x::_) -> x)

let scalar_consts_of_polarity n m = function
  | Ctypes.PosB k -> [UpperBound (n + m*k)]
  | _             -> []

let scalar_consts_of_index = function
  | Ctypes.Index.IBot            -> []
  | Ctypes.Index.IInt n          -> [Offset n] 
  | Ctypes.Index.ISeq (n, m, po) -> [Offset n;  Periodic (n, m)] ++ (scalar_consts_of_polarity n m po)

let expr_of_int = fun c -> Ast.eCon (Ast.Constant.Int c)

let quals_of_const f c =
  let t  = Ast.Sort.t_int in 
  let v  = Ast.Symbol.value_variable t in
  let ec = Ast.eCon (Ast.Constant.Int c) in
  let a  = () |> Ast.Symbol.mk_wild |> Ast.eVar in
  [ec;  (Ast.eBin (a, Ast.Plus, ec))]
  |> List.map (f v) 
  |> List.map (Q.create v t)

let quals_of_scalar_const : scalar_const -> Q.t list = function
  | Offset c -> (* v = c, v = _ + c *)
      quals_of_const (fun v e -> Ast.pEqual (Ast.eVar v, e)) c
  | UpperBound c -> (* v < c, v < _ + c *)
      quals_of_const (fun v e -> Ast.pAtom (Ast.eVar v, Ast.Lt, e)) c
  | Periodic (c, d) -> (* TODO: MODZ_c_d(v), MODZ_c_d(v - _) *)
      []

let dump_quals_to_file (fname: string) (qs: Q.t list) : unit = 
  failwith "TBD"

let scalar_quals_of_file cil = 
  cil 
  |> type_decs_of_file
  |> Misc.map (Misc.uncurry Genspec.spec_of_type)
  |> Misc.flap (fun (ct, st) -> Ctypes.I.CType.refinements_of_t ct ++ Ctypes.I.Store.indices_of_t st)
  |> Misc.flap scalar_consts_of_index
  |> Misc.flap quals_of_scalar_const 
  |> Misc.sort_and_compact
  |> (++) (FI.quals_of_file (Co.get_lib_squals ()))
  >> dump_quals_to_file (!Co.liquidc_file_prefix ^ ".squals")

(*
(1) <bas> MODZ_c_d(c)
(2) <ind> forall x,y,c,d: MODZ_c_d(x) and y = x + d => MODZ_c_d(y)
(3) <ind> forall x,y,c,d: MODZ_c_d(x) and y = x - d => MODZ_c_d(y)
*)

(***************************************************************************)
(********************* Convert Fix Solution To Indices *********************)
(***************************************************************************)

let scalar_soln_of_fix_soln (s: FixConstraint.soln) : Ix.t VM.t = 
  failwith "TBD"

(***************************************************************************)
(************************ Generate Scalar Constraints **********************)
(***************************************************************************)

let generate spec tgr gnv scim : Ci.t =
  ([], [], [])
  |> Ci.create  
  |> ConsVisitor.cons_of_scis tgr gnv FI.refstore_empty scim None

(***************************************************************************)
(*************************** Solve Scalar Constraints **********************)
(***************************************************************************)

let solve cil ci : Ix.t VM.t = 
  cil 
  |> scalar_quals_of_file 
  |> Misc.flip (Ci.solve ci) (!Co.liquidc_file_prefix ^ ".scalar")
  |> (fst <+> scalar_soln_of_fix_soln)

(***************************************************************************)
(*********************************** API ***********************************)
(***************************************************************************)

let scalarinv_of_scim cil spec tgr gnv ci =
  ci 
  >> FI.annot_clear 
  |> generate spec tgr gnv 
  |> solve cil 
  >> FI.annot_clear

