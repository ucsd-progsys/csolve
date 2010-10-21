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
module VM = CM.VarMap
module Sy = A.Symbol
module Su = A.Subst
module FI = FixInterface
module SM = Misc.StringMap
module YM = A.Symbol.SMap
module ST = Ssa_transform
module Ct = Ctypes
module Ix = Ct.Index
module Co = Constants
module P  = A.Predicate 
module Q  = A.Qualifier

module Ci = Consindex
module E  = Errormsg

open Misc.Ops

type scalar_const = Offset of int | UpperBound of int | Periodic of int * int

(***************************************************************************)
(******************** Meta Qualifiers for Scalar Invariants ****************)
(***************************************************************************)

let into_of_expr = function A.Con (A.Constant.Int i), _  -> Some i | _ -> None

let index_of_ctype ct =
  match Ctypes.I.CType.refinements_of_t ct with
  | [ix] -> ix
  | _    -> assertf "Scalar.index_of_ctype"

let value_var       = A.Symbol.value_variable A.Sort.t_int
let const_var       = A.Symbol.mk_wild ()
let param_var       = A.Symbol.mk_wild ()


let p_v_r_c         = fun r -> A.pAtom (A.eVar value_var, r, A.eVar const_var)

(* v = c *)
let p_v_eq_c        = p_v_r_c A.Eq
(* v < c *)
let p_v_lt_c        = p_v_r_c A.Lt
(* v >= c *)
let p_v_ge_c        = p_v_r_c A.Ge

let p_v_r_x_plus_c r =  
  A.pAtom (A.eVar value_var, r, A.eBin (FI.eApp_bbegin (A.eVar value_var), A.Plus, A.eVar const_var))

(* v = SKOLEM[_] + c *)
let p_v_eq_x_plus_c = p_v_r_x_plus_c A.Eq 
(* v < SKOLEM[_] + c *)
let p_v_lt_x_plus_c = p_v_r_x_plus_c A.Lt 
(* v >= SKOLEM[_] + c *)
let p_v_ge_x_plus_c = p_v_r_x_plus_c A.Ge

let quals_of_pred p = List.map (fun t -> Q.create value_var t p) [A.Sort.t_int]


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

let scalar_consts_of_polarity n m = function
  | Ct.PosB k -> [UpperBound (n + m*k)]
  | _         -> []


(* WITH SKOLEMS 
let subst_of_k_c = fun k c -> Su.of_list [(const_var, A.eInt c); (param_var, k)]

let preds_of_scalar_const = function
  | k, Offset c ->
      [p_v_eq_c; p_v_ge_c; p_v_eq_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (subst_of_k_c k c) 
      
  | k, UpperBound c ->
      [p_v_lt_c; p_v_lt_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (subst_of_k_c k c)
  
  | k, Periodic (c, d) -> (* TODO: MODZ_c_d(v), MODZ_c_d(v - _) *)
      []
*)

let preds_of_scalar_const = function
  | Offset c ->
      [p_v_eq_c; p_v_ge_c; p_v_eq_x_plus_c; p_v_ge_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (Su.of_list [const_var, A.eInt c])
      
  | UpperBound c ->
      [p_v_lt_c; p_v_lt_x_plus_c] 
      |>: (Misc.flip A.substs_pred) (Su.of_list [const_var, A.eInt c])
  
  | Periodic (c, d) -> (* TODO: MODZ_c_d(v), MODZ_c_d(v - _) *)
      []


(* AXIOMS for MODZ
(1) <bas> MODZ_c_d(c)
(2) <ind> forall x,y,c,d: MODZ_c_d(x) and y = x + d => MODZ_c_d(y)
(3) <ind> forall x,y,c,d: MODZ_c_d(x) and y = x - d => MODZ_c_d(y)
*)

let dump_quals_to_file (fname: string) (qs: Q.t list) : unit = 
  let oc  = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "@[%a@]\n" (Misc.pprint_many true "\n" Q.print) qs;
  close_out oc

let scalar_consts_of_stride = fun i -> Offset i

let scalar_consts_of_typedecs_stride tdecs =
  tdecs
  |> Misc.map_partial (function (_, t) when Cil.isPointerType t -> Some t | _ -> None)
  |>: (Cil.unrollType <+> CM.ptrRefType <+> CilMisc.bytesSizeOf)
  |> Misc.sort_and_compact
  |>: scalar_consts_of_stride 

let scalar_consts_of_index = function
  | Ix.IBot            -> []
  | Ix.IInt n          -> [Offset n] 
  | Ix.ISeq (n, m, po) -> [Offset n;  Periodic (n, m)] ++ (scalar_consts_of_polarity n m po)
 
let scalar_consts_of_typedecs_genspec tdecs =
  tdecs
  |> Misc.map (Misc.uncurry Genspec.spec_of_type)
  |> Misc.flap (fun (ct, st) -> [index_of_ctype ct] ++ Ctypes.I.Store.indices_of_t st)
  |> Misc.flap scalar_consts_of_index

let scalar_consts_of_typedecs cil = 
  cil 
  |> type_decs_of_file
  |> (fun tdecs -> scalar_consts_of_typedecs_genspec tdecs ++ scalar_consts_of_typedecs_stride tdecs)
  |> Misc.sort_and_compact


let scalar_consts_of_code cil =
  let xr = ref [] in
  let _  = CilMisc.iterConsts cil (fun c -> xr := Cil.Const c :: !xr) in
  !xr 
  |> Misc.sort_and_compact 
  |> List.map CilInterface.expr_of_cilexp  
  |> Misc.map_partial into_of_expr
  |> List.map (fun i -> Offset i)

let scalar_quals_of_file cil =
  let c1s = scalar_consts_of_typedecs cil in 
  let c2s = scalar_consts_of_code cil in
  let cs  = Misc.sort_and_compact (c1s ++ c2s) in
  (* let ks  = FI.get_skolems () |> (function [] -> [A.eInt 0] | xs -> xs) in
     Misc.cross_product ks *) 
  cs
  |> Misc.flap preds_of_scalar_const 
  |> Misc.flap quals_of_pred
  |> (++) (FI.quals_of_file (Co.get_lib_squals ()))
  >> dump_quals_to_file (!Co.liquidc_file_prefix ^ ".squals")

(***************************************************************************)
(********************** Convert Predicates To Indices **********************)
(***************************************************************************)

(* 
let unify_pred p q =
  A.unify_pred p q
  >> Misc.maybe_iter (fun su -> ignore <| Format.printf "unify_pred: p is %a, q is %a, subst = %a \n" 
                                          P.print p P.print q Su.print su)

let ppp = A.substs_pred p_v_eq_c (Su.of_list [const_var, A.eInt 0])
let _   = unify_pred p_v_eq_c ppp 
*)


let const_of_subst su =
  su |> A.Subst.to_list
     |> Misc.do_catch (Format.sprintf "Scalar.const_of_subst") (List.assoc const_var)
     |> into_of_expr
     (* |> (function A.Con (A.Constant.Int i), _  -> Some i | _ -> None) *)

let consts_of_preds p v ps =
  let p = [value_var, A.eVar v] |> A.Subst.of_list |> A.substs_pred p in
  ps |> Misc.map_partial (A.unify_pred p)
     |> Misc.map_partial const_of_subst

let indexo_of_preds_iint v ps =
  [p_v_eq_c; p_v_eq_x_plus_c]
  |> Misc.flap (fun q -> consts_of_preds q v ps) 
  |> (function c::cs -> Some (Ix.IInt (List.fold_left min c cs)) | _ -> None)

let indexo_of_preds_lowerbound v ps =
  [p_v_ge_c; p_v_ge_x_plus_c]
  |> Misc.flap (fun q -> consts_of_preds q v ps) 
  |> (function c::cs -> Some (Ix.ISeq (List.fold_left max c cs, 1, Ct.Pos)) | _ -> None)

(* {{{
let indexo_of_preds_iint v ps =
  let p_v_eq_c = [value_var, A.eVar v] |> A.Subst.of_list |> A.substs_pred p_v_eq_c in
  ps |> Misc.map_partial (A.unify_pred p_v_eq_c)
     |> Misc.map_partial const_of_subst
     |> (function [] -> None | c::cs -> Some (Ix.IInt (List.fold_left min c cs)))


try indexo_of_preds_iint v ps with ex ->
  try indexo_of_preds_iint v ps with ex ->
    (Printf.printf "indexo_of_preds_iseq v = %s, ps =%s" 
    (Sy.to_string v)
    (P.to_string (A.pAnd ps));
    raise ex)
}}}  *)

let indexo_of_preds_iseqb v ps = 
  None (* TODO *)

let indexo_of_preds_iseq  v ps = 
  None (* TODO *) 

let index_of_pred v (cr, p) = 
  let vv  = FI.name_of_varinfo v in
  [ indexo_of_preds_iint vv
  ; indexo_of_preds_iseqb vv
  ; indexo_of_preds_iseq vv 
  ; indexo_of_preds_lowerbound vv] 
  |> Misc.maybe_chain (A.conjuncts p) Ix.top
  >> (fun ix -> E.log "Scalar.index_of_pred: v = %s, cr = %a, p = %s, ix = %a \n" 
                v.Cil.vname FI.d_refctype cr (P.to_string p) Ix.d_index ix)
(* *)

(***************************************************************************)
(************************ Generate Scalar Constraints **********************)
(***************************************************************************)

let generate tgr gnv scim : Ci.t =
  ([], [], [], [])
  |> Ci.create  
  |> ConsVisitor.cons_of_scis tgr gnv FI.refstore_empty scim None

(***************************************************************************)
(*************************** Solve Scalar Constraints **********************)
(***************************************************************************)

let solve cil ci = 
  (scalar_quals_of_file cil) 
  |> Ci.force ci (!Co.liquidc_file_prefix^".scalar")
  |> SM.map (VM.mapi index_of_pred)

(***************************************************************************)
(***** Close with Bindings for Params, Undef Vars Scalar Constraints *******)
(***************************************************************************)

let ix_binds_of_spec spec fn : (string * Ix.t) list =
  spec |> FI.cspec_of_refspec 
       |> Ctypes.I.Spec.get_fun fn
       |> fst
       |> (fun x -> x.Ctypes.args) 
       |> List.map (Misc.app_snd (index_of_ctype))
 
let close_formals args (formals : Cil.varinfo list) : Ix.t VM.t -> Ix.t VM.t = 
  formals
  |> List.map (fun v -> (v, List.assoc v.Cil.vname args))
  |> CM.vm_of_list 
  |> VM.fold (fun k v acc -> VM.add k v acc)

let close_locals locals vm =
  locals 
  |> List.filter (fun v -> not (VM.mem v vm)) 
  |> List.fold_left (fun vm v -> if VM.mem v vm then vm else VM.add v Ix.top vm) vm

let close scim spec sim =
  SM.mapi begin fun fn vm ->
    if fn = Co.global_name then vm else
      let _    = asserti (SM.mem fn scim) "Scalar.close: function %s missing from scim" fn in
      let fdec = (SM.find fn scim).ST.fdec in
      let args = ix_binds_of_spec spec fn in
      (vm |> close_formals args fdec.Cil.sformals 
          |> close_locals fdec.Cil.slocals)
  end sim

(***************************************************************************)
(*********************************** API ***********************************)
(***************************************************************************)

let scalarinv_of_scim cil spec tgr gnv scim =
  scim 
  >> FI.annot_clear 
  |> generate tgr gnv 
  |> solve cil
  |> close scim spec
  >> FI.annot_clear

(***************************************************************************)
(************************* TESTING SCALAR INVS *****************************)
(***************************************************************************)

type scalar_error = 
  | MissingFun of string 
  | MissingVar of string * Cil.varinfo * Ix.t 
  | DiffIndex of string * Cil.varinfo * Ix.t * Ix.t

let d_scalar_error () = function
  | MissingFun fn -> 
      Pretty.dprintf "[SCALAR ERROR in %s] Missing Function" fn
  | MissingVar (fn, v, ix) -> 
      Pretty.dprintf "[SCALAR ERROR in %s] Missing Variable %s: inferctypes=%a" fn 
      v.Cil.vname Ix.d_index ix
  | DiffIndex (fn, v, ix, ix') ->
      Pretty.dprintf "[SCALAR ERROR in %s] Different Index %s: inferctypes=%a vs scalar=%a" fn 
      v.Cil.vname Ix.d_index ix Ix.d_index ix' 

let check_index oc fn v ix ix' =
  Pretty.fprintf oc "%s : inferctypes = %a, scalar = %a \n" 
  v.Cil.vname Ix.d_index ix Ix.d_index ix';
  Ix.is_subindex ix' ix   (* ix' ==> ix *)

let check_scalar uvm shm sim = 
  let oc  = open_out (!Co.liquidc_file_prefix ^ ".scalarlog") in
  let ppf = Format.formatter_of_out_channel oc in
  (SM.fold begin fun fn { Shape.vtyps = vcts } errs ->
    let vcts = List.filter (fun (v, _) -> VM.mem v uvm) vcts in
    if not (SM.mem fn sim) then (MissingFun fn :: errs) else 
      let im = SM.find fn sim in
      List.fold_left begin fun errs (v, ct) ->
        let ix = index_of_ctype ct in
        if ix = Ix.IBot then errs else
        if (not (VM.mem v im)) then (MissingVar (fn, v, ix) :: errs) else
          let ix'  = VM.find v im in
          if check_index oc fn v ix ix' then errs else (DiffIndex (fn, v, ix, ix')) ::errs
      end errs vcts 
   end shm [])
  >> (fun _ -> close_out oc)

let dump_quals_to_file (fname: string) (qs: Q.t list) : unit = 
  let oc  = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "@[%a@]\n" (Misc.pprint_many true "\n" Q.print) qs;
  close_out oc

(* API *) 
let test cil spec tgr gnv scim shm = 
  let sim = scalarinv_of_scim cil spec tgr gnv scim in
  let uvm = ref VM.empty in
  let _   = CM.iterUsedVars cil (fun v -> uvm := VM.add v () !uvm) in
  check_scalar !uvm shm sim
  >> (List.iter (fun e -> E.warn "%a \n" d_scalar_error e |> ignore))
  >> (fun _ -> E.log "DONE: scalar testing \n")
  |> (function [] -> exit 0 | _ -> exit 1)
