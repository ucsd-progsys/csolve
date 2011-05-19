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

module A  = Ast
module CM = CilMisc
module CI = CilInterface

module VM = CM.VarMap
module Sy = A.Symbol
module Su = A.Subst
module FA = FixAstInterface
module SM = Misc.StringMap
module YM = A.Symbol.SMap

module Ct = Ctypes
module Ix = Ct.Index
module Co = Constants
module P  = A.Predicate 
module Q  = A.Qualifier
module E  = Errormsg

open Misc.Ops

type scalar_const = 
  | Offset      of int 
  | UpperBound  of int 
  | Period      of int 

let is_valid_scalar_const = function
  | UpperBound i 
  | Offset i     when i >= 0 -> true
  | Period i     when i >= 2 -> true
  | _                        -> false

(***************************************************************************)
(******************** Meta Qualifiers for Scalar Invariants ****************)
(***************************************************************************)

let value_var       = A.Symbol.value_variable A.Sort.t_int
let const_var       = A.Symbol.mk_wild ()
let param_var       = A.Symbol.mk_wild ()
let period_var      = A.Symbol.mk_wild ()

let p_v_r_c         = fun r -> A.pAtom (A.eVar value_var, r, A.eVar const_var)

(* v = c *)
let p_v_eq_c        = p_v_r_c A.Eq
(* v < c *)
let p_v_lt_c        = p_v_r_c A.Lt
(* v <= c *)
let p_v_le_c        = p_v_r_c A.Le
(* v >= c *)
let p_v_ge_c        = p_v_r_c A.Ge

(* (v - c) mod k == 0 *) 
let p_v_minus_c_eqz_mod_k = 
  A.pEqual (A.eBin (A.eBin (A.eVar value_var, A.Minus, A.eVar const_var) ,A.Mod, A.eVar period_var)
           ,A.zero)

let p_v_r_x_plus_c r = A.pAtom (A.eVar value_var, r, A.eBin (FA.eApp_bbegin (A.eVar value_var), A.Plus, A.eVar const_var))

(* v =  BB(v) + c *)
let p_v_eq_x_plus_c = p_v_r_x_plus_c A.Eq 

(* v <  BB(v) + c *)
let p_v_lt_x_plus_c = p_v_r_x_plus_c A.Lt 

(* v >= BB(v) + c *)
let p_v_ge_x_plus_c = p_v_r_x_plus_c A.Ge

(* v <= BB(v) + c *)
let p_v_le_x_plus_c = p_v_r_x_plus_c A.Le

(* (v - BB(v) - c) mod k == 0 *) 
let p_v_minus_x_minus_c_eqz_mod_k = 
  A.pEqual (A.eBin (A.eBin (A.eVar value_var, 
                            A.Minus, (A.eBin (FA.eApp_bbegin (A.eVar value_var), A.Plus, A.eVar const_var)))
                   , A.Mod, A.eVar period_var)
           ,A.zero)

let quals_of_pred p = List.map (fun t -> Q.create "SCALAR" value_var t p) [A.Sort.t_int]

(***************************************************************************)
(***************** Convert Predicates/Refinements To Indices ***************)
(***************************************************************************)

let bind_of_subst var =
  A.Subst.to_list
  <+> Misc.do_catch (Format.sprintf "Scalar.bind_of_subst") (List.assoc var)
  <+> A.into_of_expr

let substs_of_preds p v ps =
  let p = [value_var, A.eVar v] |> A.Subst.of_list |> A.substs_pred p in
  ps |> Misc.map_partial (A.unify_pred p)

let indexo_of_preds_iint v ps =
  [p_v_eq_c; p_v_eq_x_plus_c]
  |> Misc.flap (fun q -> substs_of_preds q v ps) 
  |> Misc.map_partial (bind_of_subst const_var) 
  |> (function c::cs -> Some (Ix.IInt (List.fold_left min c cs)) | _ -> None)

let lowerboundo_of_preds v ps = 
  [p_v_ge_c; p_v_ge_x_plus_c]
  |> Misc.flap (fun q -> substs_of_preds q v ps)
  |> Misc.map_partial (bind_of_subst const_var) 
  |> (function c::cs -> Some (List.fold_left max c cs) | _ -> None)

let upperboundo_of_preds v ps = 
  [p_v_le_c; p_v_le_x_plus_c]
  |> Misc.flap (fun q -> substs_of_preds q v ps)
  |> Misc.map_partial (bind_of_subst const_var) 
  |> (function c::cs -> Some (List.fold_left min c cs) | _ -> None)

let periodo_of_preds v ps =
  [p_v_minus_x_minus_c_eqz_mod_k; p_v_minus_c_eqz_mod_k]
  |> Misc.flap (fun q -> substs_of_preds q v ps)
  |> List.map  (bind_of_subst const_var <*> bind_of_subst period_var)
  |> Misc.map_partial (function (Some c, Some k) -> Some (k, c) | _ -> None)
  |> (function x::xs -> Some (List.fold_left max x xs) | _ -> None)
  |> Misc.maybe_map (fun (k, c) -> (c mod k, k)) 

(* pmr: Hack. We should just gather up all the indices implies by the preds and
        GLB them together. *)
let indexo_of_preds_iseq v ps = 
  match periodo_of_preds v ps, lowerboundo_of_preds v ps, upperboundo_of_preds v ps with
  | Some (c, k), Some c', Some u ->
    let lb = c' + ((k - ((c' - c) mod k)) mod k) in
    let ub = u - ((u - c) mod k) in
      Some (Ix.ICClass {Ix.lb = Some lb; Ix.ub = Some ub; Ix.m = k; Ix.c = lb mod k})
  | Some (c, k), Some c', _ ->
    let lb = c' + ((k - ((c' - c) mod k)) mod k) in
      Some (Ix.ICClass {Ix.lb = Some lb; Ix.ub = None; Ix.m = k; Ix.c = lb mod k})
  | Some (c, k), _, _ ->
      Some (Ix.ICClass {Ix.lb = None; Ix.ub = None; Ix.m = k; Ix.c = c})
  | _ -> None

let indexo_of_preds_lowerbound v ps =
  lowerboundo_of_preds v ps
  |> Misc.maybe_map (fun c -> Ix.ICClass {Ix.lb = Some c; Ix.ub = None; Ix.m = 1; Ix.c = 0})

let indexo_of_preds_iseqb v ps = 
  None (* TODO *)

(* API *)
let index_of_pred v (cr, p) =
  let vv  = FA.name_of_varinfo v in
  [ indexo_of_preds_iint vv
  ; indexo_of_preds_iseqb vv
  ; indexo_of_preds_iseq vv 
  ; indexo_of_preds_lowerbound vv ]
  |> Misc.maybe_chain (A.conjuncts p) Ix.top
  >> (fun ix -> E.log "Scalar.index_of_pred: v = %s, cr = %a, p = %s, ix = %a \n" 
                v.Cil.vname Ct.d_refctype cr (P.to_string p) Ix.d_index ix)
 


let pred_of_bcc_raw p_lb p_ub p_pd bcc =
  let plb = match bcc.Ix.lb with None -> A.pTrue | Some lb -> 
              Su.of_list [(const_var, A.eInt lb)]
              |> A.substs_pred p_lb in 
  let pub = match bcc.Ix.ub with None -> A.pTrue | Some ub -> 
              Su.of_list [(const_var, A.eInt ub)] 
              |> A.substs_pred p_ub in
  let ppd = Su.of_list [(const_var, A.eInt bcc.Ix.c); (period_var, A.eInt bcc.Ix.m)] 
              |> A.substs_pred p_pd in
  A.pAnd [plb; pub; ppd]

let pred_of_bcc_int = pred_of_bcc_raw p_v_ge_c p_v_le_c p_v_minus_c_eqz_mod_k 
let pred_of_bcc_ref = pred_of_bcc_raw p_v_ge_x_plus_c p_v_le_x_plus_c p_v_minus_x_minus_c_eqz_mod_k


(*
let pred_of_bcc_int bcc =
  let plb = match bcc.Ix.lb with None -> A.pTrue | Some lb -> 
              Su.of_list [(const_var, A.eInt lb)]
              |> A.substs_pred p_v_ge_c in 
  let pub = match bcc.Ix.ub with None -> A.pTrue | Some ub -> 
              Su.of_list [(const_var, A.eInt ub)] 
              |> A.substs_pred p_v_le_c in
  let ppd = Su.of_list [(const_var, A.eInt bcc.Ix.c); (period_var, A.eInt bcc.Ix.m)] 
              |> A.substs_pred p_v_minus_c_eqz_mod_k in
  A.pAnd [plb; pub; ppd]
*)

let pred_of_index_int = function
  | Ix.IBot        -> value_var, A.pFalse
  | Ix.IInt n      -> value_var, A.pEqual (A.eVar value_var, A.eInt n)
  | Ix.ICClass bcc -> value_var, pred_of_bcc_int bcc
  | _              -> value_var, A.pTrue 

let pred_of_index_ref = function
  | Ix.IBot        -> value_var, A.pFalse
  | Ix.IInt n      -> value_var, A.substs_pred p_v_eq_x_plus_c (Su.of_list [const_var, A.eInt n])
  | Ix.ICClass bcc -> value_var, pred_of_bcc_ref bcc
  | _              -> value_var, A.pTrue 

(* API *)
let pred_of_ctype = function
  | Ct.Ref (_, ix) -> pred_of_index_ref ix
  | Ct.Int (_, ix) -> pred_of_index_int ix

(*
let pred_of_index = function
  | Ix.IBot        -> value_var, A.pFalse
  | Ix.ICClass bcc -> value_var, pred_of_bcc bcc
  | _              -> value_var, A.pTrue 
*)


(***************************************************************************)
(************************* Scrape Scalar Qualifiers ************************)
(***************************************************************************)

let hash_of_ciltype t = 
  Pretty.dprintf "%a ### %a " Cil.d_typsig (Cil.typeSig t) Cil.d_attrlist (Cil.typeAttrs t)
  |> Pretty.sprint ~width:80

let type_decs_of_file (cil: Cil.file) : (Cil.location * Cil.typ) list =
  let x = ref [] in 
  CM.iterDefVars cil begin fun v -> match v.Cil.vtype with 
    | Cil.TFun (t,_,_,_) | t -> x := (v.Cil.vdecl, t) :: !x
  end; 
  !x 
  |> Misc.kgroupby (snd <+> hash_of_ciltype)
  |> Misc.map (function (_,x::_) -> x)

let scalar_consts_of_upper_bound m = function
  | Some k -> [UpperBound (k)]
  | _      -> []

let bound_preds_of_scalar_const = function
  | Offset c ->
      [p_v_eq_c; p_v_ge_c; p_v_eq_x_plus_c; p_v_ge_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (Su.of_list [const_var, A.eInt c])
  | UpperBound c ->
      [p_v_lt_c; p_v_lt_x_plus_c; p_v_le_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (Su.of_list [const_var, A.eInt c])
  | _ -> [] 

let bound_pred_of_scalar_const p = function
  | Offset c -> [A.substs_pred p (Su.of_list [const_var, A.eInt c])]
  | _        -> []
  
let period_preds_of_scalar_consts cs =
  let os = Misc.map_partial (function Offset c -> Some c | _ -> None) cs in
  let ks = Misc.map_partial (function Period k when k < 50 -> Some k | _ -> None) cs in
  ks |> Misc.flap (fun k -> os |>: (fun o -> (o mod k), k))
     |> Misc.sort_and_compact 
     |> List.map  (fun (o, k) -> Su.of_list [(const_var, A.eInt o); (period_var, A.eInt k)])
     |> Misc.flap (fun su -> [A.substs_pred p_v_minus_x_minus_c_eqz_mod_k su; A.substs_pred p_v_minus_c_eqz_mod_k su])

let preds_of_scalar_consts cs = 
  (Misc.flap bound_preds_of_scalar_const cs) ++ (period_preds_of_scalar_consts cs)

let dump_quals_to_file fname qs = 
  let oc  = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  let _   = Printf.printf "[SCALAR] Auto-generated %n quals\n" (List.length qs) in
  Format.fprintf ppf "@[%a@]\n" (Misc.pprint_many true "\n" Q.print) qs;
  close_out oc

let scalar_consts_of_typedecs_stride =
      Misc.map_partial (function (_, t) when Cil.isPointerType t -> Some t | _ -> None)
  <+> Misc.map (Cil.unrollType <+> CM.ptrRefType <+> CM.bytesSizeOf)
  <+> Misc.sort_and_compact
  <+> Misc.flap (fun i -> [Offset i; Period i])

let scalar_consts_of_index = function
  | Ix.ICClass bcc -> 
         [Offset bcc.Ix.c; Period bcc.Ix.m] 
      ++ (match bcc.Ix.lb with Some lb -> [Offset lb] | _ -> [])
      ++ (scalar_consts_of_upper_bound bcc.Ix.m bcc.Ix.ub)
  | Ix.IInt n  -> [Offset n] 
  | _          -> []

let scalar_consts_of_typedecs_genspec =
      Misc.map (Misc.uncurry Genspec.spec_of_type)
  <+> Misc.flap (fun (ct, st) -> [Ct.index_of_ctype ct] ++ Ct.I.Store.indices_of_t st)
  <+> (fun is -> List.iter (ignore <.> E.log "[SCALAR] index = %a \n" Ix.d_index) is; is)  
  <+> Misc.flap scalar_consts_of_index

let scalar_consts_of_typedecs = 
      type_decs_of_file
  <+> (scalar_consts_of_typedecs_genspec <*> scalar_consts_of_typedecs_stride)
  <+> Misc.uncurry (++)
  <+> Misc.sort_and_compact

(* DO NOT DELETE
let scalar_consts_of_code cil =
  let xr = ref [] in
  let _  = CM.iterExprs cil (function Cil.Const c -> xr := CI.expr_of_cilcon c :: !xr; false | _ -> true) in
  !xr 
  |> Misc.map_partial A.into_of_expr
  |> Misc.sort_and_compact 
  |> List.map (fun i -> Offset i)

let increments_of_code cil =
  let xr = ref [] in
  let _  = CM.iterExprs cil begin function 
             | Cil.BinOp (Cil.PlusPI, e, Cil.Const c, _) -> xr := (e, c) :: !xr; false 
             | _ -> true end in
  !xr 
  |> Misc.map_partial begin fun (e, c) -> 
       match c |> CI.expr_of_cilcon |> A.into_of_expr with 
       | Some i -> Some (i * (CI.stride_of_cilexp e))
       | _      -> None
     end
  |> Misc.sort_and_compact 
  |> List.map (fun i -> Offset i)
*)



let scalar_consts_of_file cil = 
  [ scalar_consts_of_typedecs
(*; scalar_consts_of_code
  ; increments_of_code *) ]
  |> Misc.flap (fun f -> f cil) 
  |> List.filter is_valid_scalar_const

(* API *)
let scalar_quals_of_file cil =
  cil
  |> scalar_consts_of_file
  |> Misc.sort_and_compact  
  |> preds_of_scalar_consts 
  |> Misc.flap quals_of_pred
  |> (++) (FA.quals_of_file (Co.get_lib_squals ()))
  >> dump_quals_to_file (!Co.liquidc_file_prefix ^ ".squals")
