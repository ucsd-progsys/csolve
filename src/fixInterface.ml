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

(************** Interface between LiquidC and Fixpoint *************)

module IM = Misc.IntMap
module F  = Format
module ST = Ssa_transform
module  C = FixConstraint

module  A = Ast
module  P = A.Predicate
module  E = A.Expression
module Sy = A.Symbol
module Su = A.Subst
module So = A.Sort
module  Q = A.Qualifier

module CI = CilInterface
module Ct = Ctypes
module It = Ct.I
module YM = Sy.SMap

module SM = Misc.StringMap
module Co = Constants
module CM = CilMisc
module VM = CM.VarMap
module Ix = Index
module RCt = Ctypes.RefCTypes
module FA  = FixAstInterface
module Sc  = ScalarCtypes

module ES  = Ctypes.EffectSet
module ED  = EffectDecls

module M   = Misc

open Misc.Ops
open Cil

let mydebug = false

type cilenv   = Ct.refcfun SM.t * Ct.refctype YM.t * FA.name YM.t

(******************************************************************************)
(***************************** Tags and Locations *****************************)
(******************************************************************************)

let (fresh_tag, _ (* loc_of_tag *) ) =
  let tbl     = Hashtbl.create 17 in
  let fint, _ = Misc.mk_int_factory () in
    ((fun loc ->
        let t = fint () in
          Hashtbl.add tbl t loc;
          t),
     (fun t -> Hashtbl.find tbl t))

(*******************************************************************)
(********************* CLOC-to-ALOC Maps Names *********************)
(*******************************************************************)

type alocmap = Sloc.t -> Sloc.t option 

let reft_of_reft r t' =
  let vv   = C.vv_of_reft r in
  let t    = C.sort_of_reft r in
  let _    = asserti (So.ptr_of_t t  <> None) "reft_of_reft (src)" in
  let _    = asserti (So.ptr_of_t t' <> None) "reft_of_reft (dst)" in
  let vv'  = Sy.value_variable t' in
  let evv' = A.eVar vv' in
  r |> C.ras_of_reft 
    |> List.map (function C.Conc p      -> C.Conc (P.subst p vv evv') 
                        | C.Kvar (s, k) -> C.Kvar (Su.extend s (vv, evv'), k))
    |> C.make_reft vv' t'
(*    >> F.printf "reft_of_reft: r = %a, t' = %a, r' = %a \n" (C.print_reft None) r So.print t' (C.print_reft None) 
*)

let sort_of_prectype = function
  | Ct.Ref (l,_) -> FA.so_ref l
  | _            -> FA.so_int

let spec_sort_of_prectype = function
  | Ct.Ref _ -> FA.so_ref Sloc.none
  | _        -> FA.so_int

let refctype_of_reft_ctype r = function
  | Ct.Int (w,k) -> Ct.Int (w, (k, r)) 
  | Ct.Ref (l,o) -> Ct.Ref (l, (o, reft_of_reft r (FA.so_ref l)))

let refctype_of_reft_ctype r = function
  | Ct.Int (w,k) -> Ct.Int (w, (k, r)) 
  | Ct.Ref (l,o) -> Ct.Ref (l, (o, reft_of_reft r (FA.so_ref l)))

let spec_refctype_of_reft_ctype r = function
  | Ct.Int (w,k) -> Ct.Int (w, (k, r)) 
  | Ct.Ref (l,o) -> Ct.Ref (l, (o, r))

let replace_reft r = function
  | Ct.Int (w, (i, _))  -> Ct.Int (w, (i, r))
  | Ct.Ref (l, (i, _))  -> Ct.Ref (l, (i, reft_of_reft r (FA.so_ref l)))

let refctype_of_ctype f = function
  | Ct.Int (i, x) as t ->
      let r = C.make_reft FA.vv_int So.t_int (f t) in
      Ct.Int (i, (x, r)) 
  | Ct.Ref (l, x) as t ->
      let so = FA.so_ref l in
      let vv = Sy.value_variable so in
      let r  = C.make_reft vv so (f t) in
      Ct.Ref (l, (x, r)) 


let refcfun_of_cfun   = It.CFun.map (refctype_of_ctype (fun _ -> []))


let is_base = function
  | TInt _ -> true
  | _      -> false



(* API *)
let pred_of_refctype s v cr = 
  let n        = FA.name_of_varinfo v |> A.eVar in
  let vv,_,ras = cr |> Ct.reft_of_refctype |> C.apply_solution s in
  let su       = Su.of_list [(vv, n)] in
  ras |> Misc.flap (function C.Conc (A.And ps, _) -> ps | _ -> []) 
      |> A.pAnd
      |> Misc.flip A.substs_pred su

(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)


let ce_rem   = fun n cenv       -> Misc.app_snd3 (YM.remove n) cenv
let ce_mem   = fun n (_, vnv,_) -> YM.mem n vnv

let ce_find n (_, vnv, _) =
  try YM.find n vnv with Not_found -> 
    let _  = asserti false "Unknown name! %s" (FA.string_of_name n) in
    assertf "Unknown name! %s" (FA.string_of_name n)

let ce_find_fn s (fnv, _,_) =
  try SM.find s fnv with Not_found ->
    assertf "FixInterface.ce_find: Unknown function! %s" s

let ce_adds cenv ncrs =
  let _ = if mydebug then (List.iter (fun (n, cr) -> Errormsg.log "ce_adds: n = %s cr = %a \n"
  (FA.string_of_name n) Ct.d_refctype cr) ncrs) in
  let _ = List.iter (fun (n, cr) -> Annots.annot_var n cr) ncrs in
  List.fold_left begin fun (fnv, env, livem) (n, cr) ->
    let env'   = YM.add n cr env in
    let livem' = match FA.base_of_name n with 
                 | None -> livem 
                 | Some bn -> YM.add bn n livem in
    fnv, env', livem'
  end cenv ncrs

let ce_adds_fn (fnv, vnv, livem) sfrs = 
  let _ = List.iter (Misc.uncurry Annots.annot_fun) sfrs in
  (List.fold_left (fun fnv (s, fr) -> SM.add s fr fnv) fnv sfrs, vnv, livem)

let ce_mem_fn = fun s (fnv, _, _) -> SM.mem s fnv

let ce_empty = (SM.empty, YM.empty, YM.empty) 

let d_cilenv () (fnv,_,_) = failwith "TBD: d_cilenv"


let print_rctype so ppf rct =
  rct |> Ct.reft_of_refctype |> C.print_reft so ppf

let print_binding so ppf (n, rct) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_rctype so) rct

let print_ce so ppf (_, vnv) =
  YM.iter begin fun n cr -> 
    F.fprintf ppf "@[%a@]@\n" (print_binding so) (n, cr) 
  end vnv

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)


let ra_fresh        = fun _ -> [C.Kvar (Su.empty, C.fresh_kvar ())]
let ra_true         = fun _ -> []
let ra_false        = fun _ -> [C.Conc (A.pFalse)]

let ra_zero ct =
  let vv = ct |> sort_of_prectype |> Sy.value_variable in
  [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.zero))]

let ra_equal v ct =
  let vv = ct |> sort_of_prectype |> Sy.value_variable in
  [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar v))]

let ra_ptr_offset offset ct =
  let so  = sort_of_prectype ct in
  let evv = so |> Sy.value_variable |> A.eVar in
    [C.Conc
      (A.pEqual (evv, A.eBin (FA.eApp_bbegin evv, A.Plus, A.eCon (A.Constant.Int offset))))]

let ra_deref ct base offset =
  let so  = sort_of_prectype ct in
  let vv  = so |> Sy.value_variable in
  let ptr = A.eBin (base, A.Plus, A.eCon (A.Constant.Int offset)) in
    [C.Conc (A.pAtom (A.eVar vv, A.Eq, FA.eApp_deref ptr so))]

let e_cil_effect_true = Cil.one

let p_effect_var_true evar =
  A.pEqual (evar, A.eInt 1)

let ra_singleton_effect eff v ct =
  let vv     = ct |> sort_of_prectype |> Sy.value_variable |> A.eVar in
  let others = M.negfilter ((=) eff) <| ED.getEffects () in
    [C.Conc begin
      A.pAnd begin
           p_effect_var_true (eff |> ED.nameOfEffect |> A.eVar)
        :: A.pEqual (vv, A.eVar v)
        :: List.map
             (ED.nameOfEffect <+> A.eVar <+> p_effect_var_true <+> A.pNot)
             others
      end
    end]

let ra_skolem, get_skolems =
  let xr = ref 0 in
  (fun ct ->
    let vv = ct |> sort_of_prectype |> Sy.value_variable in
    [C.Conc (A.pEqual (A.eVar vv, FA.eApp_skolem (A.eInt (xr =+ 1))))]),
  (fun _ -> Misc.range 0 !xr |>: A.eInt) 

let ra_bbegin ct =
  ct 
  |> sort_of_prectype 
  |> Sy.value_variable 
  |> A.eVar 
  |> (fun vv -> [C.Conc (A.pEqual (vv, FA.eApp_bbegin vv))])

let ra_indexpred ct =
  let vv     = ct |> sort_of_prectype |> Sy.value_variable |> A.eVar in
  let vv', p = Sc.pred_of_ctype ct in
    [C.Conc (P.subst p vv' vv)]

let p_ptr_footprint vv v =
  let evv  = A.eVar vv in
  let eptr = v |> FA.name_of_varinfo |> A.eVar in
  let sz   = v.vtype |> CM.ptrRefType |> CM.bytesSizeOf |> A.eInt in
    A.pAnd [A.pEqual (FA.eApp_bbegin evv, FA.eApp_bbegin eptr);
            A.pEqual (FA.eApp_bend evv, FA.eApp_bend eptr);
            A.pEqual (FA.eApp_uncheck evv, FA.eApp_uncheck eptr);
            A.pAtom (eptr, A.Le, evv);
            A.pAtom (evv, A.Lt, A.eBin (eptr, A.Plus, sz))]

let ra_ptr_footprint env v =
  let ct   = Ct.ctype_of_refctype <| ce_find (FA.name_of_varinfo v) env in
  let so   = sort_of_prectype ct in
  let vv   = Sy.value_variable so in
    (vv, so, [C.Conc (p_ptr_footprint vv v)])

let e_aux l ra =
  refctype_of_ctype ra <| Ct.Ref (l, Ix.top)

let e_false l = e_aux l ra_false
let e_true l  = e_aux l ra_true
let e_fresh l = e_aux l ra_fresh

let t_fresh         = fun ct -> refctype_of_ctype ra_fresh ct 
let t_true          = fun ct -> refctype_of_ctype ra_true ct
let t_zero          = fun ct -> refctype_of_ctype ra_zero ct
let t_equal         = fun ct v -> refctype_of_ctype (ra_equal v) ct
let t_skolem        = fun ct -> refctype_of_ctype ra_skolem ct 
let t_ptr_offset offset = refctype_of_ctype <| ra_ptr_offset offset

let t_singleton_effect env v eff =
  let vn = FA.name_of_varinfo v in
    refctype_of_ctype (ra_singleton_effect eff vn) (Ct.ctype_of_refctype <| ce_find vn env)

let t_addr          = fun l  -> t_true <| Ct.Ref (l, Ix.top)

let t_conv_refctype      = fun f rct -> rct |> Ct.ctype_of_refctype |> refctype_of_ctype f
let t_true_refctype      = t_conv_refctype ra_true
let t_false_refctype     = t_conv_refctype ra_false
let t_zero_refctype      = t_conv_refctype ra_zero
let t_indexpred_refctype = t_conv_refctype ra_indexpred

let t_pred_aux sort_of_ct reft_ct_to_refctype ct v p =
  let so = sort_of_ct ct in
  let vv = Sy.value_variable so in
  let p  = P.subst p v (A.eVar vv) in
  let r  = C.make_reft vv so [C.Conc p] in
  reft_ct_to_refctype r ct

(* convert {v : ct | p } into refctype *)
let t_pred ct v p = t_pred_aux sort_of_prectype refctype_of_reft_ctype ct v p

(* convert {v : ct | p } into refctype; ignores underyling pointer sort *)
let t_spec_pred ct v p = t_pred_aux spec_sort_of_prectype spec_refctype_of_reft_ctype ct v p

let t_size_ptr ct size =
  let so  = sort_of_prectype ct in
  let vv  = Sy.value_variable so in
  let evv = A.eVar vv in
  t_pred ct vv
    (A.pAnd [A.pAtom (evv, A.Gt, A.zero);
             A.pAtom (FA.eApp_bbegin evv, A.Eq, evv);
             A.pAtom (FA.eApp_bend evv, A.Eq, A.eBin (evv, A.Plus, A.eCon (A.Constant.Int size)))])

let t_ptr_footprint env v =
     ce_find (FA.name_of_varinfo v) env
  |> Ct.ctype_of_refctype
  |> refctype_of_reft_ctype (ra_ptr_footprint env v)

let t_valid_ptr ct =
  let so  = sort_of_prectype ct in
  let vv  = Sy.value_variable so in
  let evv = A.eVar vv in
  t_pred ct vv (A.pOr [A.pAtom (FA.eApp_uncheck evv, A.Eq, A.one);
                       A.pAnd [A.pAtom (evv, A.Ne, A.zero);
                               A.pAtom (FA.eApp_bbegin evv, A.Le, evv);
                               A.pAtom (evv, A.Lt, FA.eApp_bend evv)]])

let is_reference cenv x =
  if YM.mem x FA.builtinm then (* TBD: REMOVE GROSS HACK *)
    false
  else if not (ce_mem x cenv) then
    false
  else match ce_find x cenv with 
    | Ct.Ref (_,(_,_)) -> true
    | _                -> false

let mk_eq_uf = fun f x y -> A.pAtom (f x, A.Eq, f y)

let t_exp_ptr cenv e ct vv so p = (* TBD: REMOVE UNSOUND AND SHADY HACK *)
  let refs = P.support p |> List.filter (is_reference cenv) in
  match ct, refs with
  | (Ct.Ref (_,_)), [x]  ->
      let x         = A.eVar x  in
      let vv        = A.eVar vv in
      let unchecked =
        if e |> typeOf |> CM.is_unchecked_ptr_type then
          C.Conc (A.pAtom (FA.eApp_uncheck vv, A.Eq, A.one))
        else
          C.Conc (mk_eq_uf FA.eApp_uncheck vv x)
      in [C.Conc (mk_eq_uf FA.eApp_bbegin  vv x);
          C.Conc (mk_eq_uf FA.eApp_bend    vv x);
          unchecked]
  | _ ->
      []


let t_exp cenv ct e =
  let so    = sort_of_prectype ct in
  let vv    = Sy.value_variable so in
  let gp, p = CI.reft_of_cilexp vv e in (* TODO: DEFERREDCHECKS *)
(* let _      = Errormsg.log "\n reft_of_cilexp [e: %a] [p: %s] \n" Cil.d_exp e (P.to_string p) in *)
  let rs    = [C.Conc p] ++ (t_exp_ptr cenv e ct vv so p) in
  let r     = C.make_reft vv so rs in
  (gp, refctype_of_reft_ctype r ct)

let ptrs_of_exp e = 
  let xm = ref VM.empty in
  let _  = CM.iterExprVars e (fun v -> xm := VM.add v () !xm) in
  !xm |> CM.vm_to_list |>: fst |> (List.filter (fun v -> CM.is_reference v.Cil.vtype))

let t_exp_scalar_ptr vv e = (* TODO: REMOVE UNSOUND AND SHADY HACK *)
  e |> ptrs_of_exp 
    |> (function [v] -> [C.Conc (mk_eq_uf FA.eApp_bbegin (A.eVar vv) (A.eVar (FA.name_of_varinfo v)))] | _ -> [])

let t_exp_scalar v e =
(*  let ct  = Ct.scalar_ctype in *)
  let ct  = Ct.vtype_to_ctype v.Cil.vtype in
  let so  = sort_of_prectype ct in
  let vv  = Sy.value_variable so in
  let _,p = CI.reft_of_cilexp vv e in
  let rs  = [C.Conc p] in
  let rb  = CM.is_reference v.Cil.vtype in 
(*  let _  = Errormsg.log "t_exp_scalar: v=%s e=%a ref=%b \n" v.Cil.vname Cil.d_exp e rb in  *)
  let rs  = if rb then (rs ++ t_exp_scalar_ptr vv e) else rs in
  let r   = C.make_reft vv so rs in
  refctype_of_reft_ctype r ct

let t_name (_,vnv,_) n = 
  let _  = asserti (YM.mem n vnv) "t_name: reading unbound var %s" (FA.string_of_name n) in
  let rct = YM.find n vnv in
  let so = rct |> Ct.reft_of_refctype |> C.sort_of_reft in
  let vv = Sy.value_variable so in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
  replace_reft r rct

(* API *)
let map_fn = RCt.CFun.map 

(* let t_fresh_fn = It.CFun.map t_fresh  *)

let t_ctype_refctype ct rct =
  rct 
  |> Ct.reft_of_refctype
  |> Misc.flip refctype_of_reft_ctype ct 

let strengthen_refctype mkreft rct =
  let reft = Ct.reft_of_refctype rct in
  let vv   = C.vv_of_reft reft in
  let so   = C.sort_of_reft reft in
  let ras  = C.ras_of_reft reft in
  replace_reft (C.make_reft vv so (mkreft rct @ ras)) rct

let refctype_subs f nzs = 
  nzs |> Misc.map (Misc.app_snd f) 
      |> Su.of_list
      |> C.theta
      |> Misc.app_snd
      |> RCt.CType.map

(* API *)
let t_subs_exps    = refctype_subs (CI.expr_of_cilexp (* skolem *))
let t_subs_names   = refctype_subs A.eVar
let refstore_subs  = fun f subs st   -> RCt.Store.map (f subs) st
let effectset_subs = fun f subs effs -> ES.apply (f subs) effs

let refstore_fresh f st =
     st
  |> RCt.Store.map t_fresh
  >> Annots.annot_sto f 

let conv_refstore_bottom st =
  RCt.Store.map_variances t_false_refctype t_true_refctype st

let conv_effectset_bottom effs =
  ES.apply t_false_refctype effs

let t_scalar_zero = refctype_of_ctype ra_bbegin Ct.scalar_ctype

(* {{{
let t_scalar_index = Sc.pred_of_index <+> Misc.uncurry (t_pred Ct.scalar_ctype)
let t_scalar = function
  | Ct.Ref (_,Ix.IInt 0) -> t_scalar_zero 
  | Ct.Int (_,ix)        -> t_scalar_index ix 
  | _                    -> t_true Ct.scalar_ctype

let t_scalar = Ct.index_of_ctype <+> 
               Sc.pred_of_index <+> 
               Misc.uncurry (t_pred Ct.nscalar_ctype)
let t_scalar_zero = t_scalar (Ct.Int (0, Ix.IInt 0))


}}} *)

(* let t_scalar = Sc.pred_of_ctype <+> Misc.uncurry (t_pred C.scalar_ctype) *)
let t_scalar ct = ct |> Sc.non_null_pred_of_ctype |> Misc.uncurry (t_pred ct)

let deconstruct_refctype rct = 
  let r = Ct.reft_of_refctype rct in
  (Ct.ctype_of_refctype rct, C.vv_of_reft r, C.sort_of_reft r, C.ras_of_reft r)

let meet_refctype rct1 rct2 = 
  let (ct1, vv1, so1, ra1) = deconstruct_refctype rct1 in
  let (ct2, vv2, so2, ra2) = deconstruct_refctype rct2 in
  if not (ct1 = ct2 && vv1 = vv2 && so1 = so2) then begin
    Pretty.printf "ct1 = %a, ct2 = %a" Ctypes.d_ctype ct1 Ctypes.d_ctype ct2;
    Pretty.printf "vv1 = %s, vv2 = %s" (Sy.to_string vv1) (Sy.to_string vv2);
    assertf "meet_refctype"  
  end;
  refctype_of_reft_ctype (C.make_reft vv1 so1 (ra1 ++ ra2)) ct1 

let vv_rename so' r =
  let vv'    = Sy.value_variable so' in
  let vv, so = r |> (C.vv_of_reft <*> C.sort_of_reft) in
  let ras'   = r |> C.theta (Su.of_list [vv, A.eVar vv']) |> C.ras_of_reft in
  C.make_reft vv' so' ras'

let t_scalar_refctype_raw rct =
(*  let so'  = Ct.scalar_ctype |> sort_of_prectype in *)
  let so' = Ct.ctype_of_refctype rct |> sort_of_prectype in
  let r'   = rct |> Ct.reft_of_refctype |> vv_rename so' in
    (* refctype_of_reft_ctype r' Ct.scalar_ctype *)
   refctype_of_reft_ctype r' (Ct.ctype_of_refctype rct)

(* API *)
let t_scalar_refctype =
  (t_scalar_refctype_raw <*> (t_scalar <.> Ct.ctype_of_refctype))
  <+> Misc.uncurry meet_refctype

(* WRAPPER  
let t_scalar_refctype x =
  x |> t_scalar_refctype
    >> (fun y -> ignore <| Pretty.printf "t_scalar_refctype: [in=%a] [out=%a] \n" Ct.d_refctype x Ct.d_refctype y)
*)

(* API *)
let t_subs_locs lsubs rct =
  rct |> RCt.CType.subs lsubs |> replace_reft (Ct.reft_of_refctype rct)


(* API *)
let rename_refctype lsubs subs cr =
  cr |> t_subs_locs lsubs
     |> t_subs_exps subs

let name_of_sloc_index l i = 
  FA.name_of_string <| Sloc.to_string l ^ "#" ^ Ix.repr i

let subs_of_lsubs lsubs sto = 
  Misc.tr_rev_flap begin fun (l, l') ->
    if not (RCt.Store.Data.mem sto l) then [] else
      let is  = l |> Ct.refstore_get sto |> RCt.LDesc.indices in
      let ns  = List.map (name_of_sloc_index l)  is in
      let ns' = List.map (name_of_sloc_index l') is in
      List.combine ns ns'
  end lsubs

let refstore_subs_locs lsubs sto =
  let subs = subs_of_lsubs lsubs sto in
    RCt.Store.map ((t_subs_locs lsubs) <+> (t_subs_names subs)) sto

let effectset_subs_locs lsubs sto effs =
  let subs = subs_of_lsubs lsubs sto in
    ES.apply ((t_subs_locs lsubs) <+> (t_subs_names subs)) effs

let subs_refctype sto lsubs substrs cr =
  let subs = List.map (Misc.map_pair FA.name_of_string) substrs ++ subs_of_lsubs lsubs sto in
    cr |> t_subs_locs lsubs
       |> t_subs_names subs


exception ContainsDeref

let expr_has_deref e = match FA.maybe_deref e with 
  | None   -> ()
  | Some _ -> raise ContainsDeref 

let pred_has_deref p = 
  try 
    P.iter (fun _ -> ()) expr_has_deref p;
    false
  with ContainsDeref -> 
    true

let may_contain_deref rct =
  match rct |> Ct.reft_of_refctype |> C.preds_kvars_of_reft with
  | _, _ :: _ -> true
  | ps, _     -> List.exists pred_has_deref ps 

(**************************************************************)
(*******************Constraint Simplification *****************)
(**************************************************************)

let is_temp_name n =
  let s = FA.string_of_name n in
  List.exists (Misc.is_substring s) [Ix.repr_prefix; "lqn#"] 


let is_var_def = function
  | C.Conc (A.Atom ((A.Var x, _), A.Eq, (A.Var y, _)), _) 
    when Sy.is_value_variable x -> Some y
  | C.Conc (A.Atom ((A.Var x, _), A.Eq, (A.Var y, _)), _) 
    when Sy.is_value_variable y -> Some x
  | _                           -> None

let str_reft env r = 
  Misc.expand begin fun (_, t, ras) ->
    ras |> Misc.map_partial is_var_def
        |> List.filter (fun x -> YM.mem x env)
        |> List.map (fun x -> YM.find x env)
        |> (fun rs -> rs, ras)
 end [r] []

let is_temp_equality ra = 
  match is_var_def ra with 
  | Some x -> is_temp_name x
  | _      -> false 

let strengthen_reft env ((v, t, ras) as r) =
  r |> str_reft env 
    |> List.filter (not <.> is_temp_equality) 
(*  |> List.filter (fun ra -> match is_var_def ra with None -> true | _ -> false) *)
    |> Misc.sort_and_compact
    |> (fun ras' -> v, t, ras')

(*******************************************************************)
(********************** Pointer Canonicization *********************)
(*******************************************************************)

let canon_sort t = 
  (match So.ptr_of_t t with
   | Some (So.Loc s) -> s |> FA.sloc_of_string |> Sloc.canonical |> FA.so_ref 
   | _               -> t)
(*  >> (fun t' -> Format.printf "canon_sort: t = %a, t' = %a \n" So.print t So.print t') *)

let canon_reft r = 
  let t  = C.sort_of_reft r in
  let t' = canon_sort t in
  if t = t' then r else reft_of_reft r t'

let canon_env env = 
  YM.map canon_reft env

let canon_refctype = function
  | Ct.Ref (l, (i, r)) -> Ct.Ref (Sloc.canonical l, (i, canon_reft r))
  | rct                -> rct

(******************************************************************************)
(********************** WF For Dereferencing Expressions **********************)
(******************************************************************************)

exception InvalidDeref

let find_unfolded_loc l sto =
  try
    RCt.Store.Data.find sto l
  with Not_found ->
    try
      l |> Sloc.canonical |> RCt.Store.Data.find sto
    with Not_found -> raise InvalidDeref

let field_of_address sto (l, i) =
  match sto |> find_unfolded_loc l |> RCt.LDesc.find i with
    | [(_, fld)] -> fld
    | _          -> raise InvalidDeref

let address_of_ref = function
  | Ct.Ref (l, (i, _)) -> (l, i)
  | _                  -> raise InvalidDeref

let rec address_of_expr cenv sto e = match E.unwrap e with
  | A.Var p            -> address_of_ref <| ce_find p cenv
  | A.Cst (e, _)       -> address_of_expr cenv sto e
  | A.Bin (e1, op, e2) ->
    let n    = match E.unwrap e2 with A.Con (A.Constant.Int n) -> n | _ -> raise InvalidDeref in
    let n    = match op with A.Plus -> n | A.Minus -> -n | _ -> raise InvalidDeref in
    let l, i = address_of_expr cenv sto e in
      (l, Index.offset n i)
  | _ ->
    begin match FA.maybe_deref e with
      | Some e -> e |> field_of_expr_address cenv sto |> RCt.Field.type_of |> address_of_ref
      | None   -> assert false
    end
  | _ -> assert false

and field_of_expr_address cenv sto e =
  e |> address_of_expr cenv sto |> field_of_address sto

let expr_derefs_wf cenv sto e = match FA.maybe_deref e with
  | Some e ->
    begin
      try
        e |> field_of_expr_address cenv sto |> RCt.Field.is_final
      with InvalidDeref -> false
    end
  | None -> true

let filter_store_derefs cenv sto rct q =
  let cenv = ce_adds cenv [(Q.vv_of_t q, rct)] in
  let wf   = ref true in
       q
    |> Q.pred_of_t
    |> P.iter (fun _ -> ()) (fun e -> wf := !wf && expr_derefs_wf cenv sto e);
    !wf

let sloc_binds_of_refldesc l rd =
  RCt.LDesc.foldn begin fun _ binds i rfld ->
    ((name_of_sloc_index l i, RCt.Field.type_of rfld), i)::binds
  end [] rd
  |> List.rev

let binds_of_refldesc l rd = 
  sloc_binds_of_refldesc l rd 
  |> List.filter (fun (_, i) -> not (Ix.is_periodic i))
  |> List.map fst

(* API *)
let is_poly_cloc st cl =
  let _ = asserts (not (Sloc.is_abstract cl)) "is_poly_cloc" in
  Ct.refstore_get st cl 
  |> binds_of_refldesc cl 
  |> (=) []

(******************************************************************************)
(************************ Address-Dependent Refinements ***********************)
(******************************************************************************)

let vv_addr      = Sy.of_string "VVADDR"
let vv_addr_expr = A.eVar vv_addr

let replace_addr v rct =
  t_subs_names [(vv_addr, FA.name_of_string v.vname)] rct

(******************************************************************************)
(*********************************** Effects **********************************)
(******************************************************************************)

let t_effect = refctype_of_ctype ra_true (Ct.Int (0, Ix.top))

let with_effects_in_env env f =
     ED.getEffects ()
  |> List.map (fun eff -> (ED.nameOfEffect eff, t_effect))
  |> ce_adds env
  |> f

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let d_ldbind () (l, ld) =
  Pretty.dprintf "[%a |-> %a]" Sloc.d_sloc l RCt.LDesc.d_ldesc ld

let d_cfbind () (l, cf) = 
  Pretty.dprintf "[%a |-> %a]" Sloc.d_sloc l RCt.CFun.d_cfun cf 

let d_ldbinds () llds = 
    Pretty.seq ~sep:(Pretty.text ",") ~doit:(d_ldbind ()) ~elements:llds

let d_lcfbinds () lcfs = 
    Pretty.seq ~sep:(Pretty.text ",") ~doit:(d_cfbind ()) ~elements:lcfs

let d_bindings () (llds, lcfs) = 
  Pretty.dprintf "DATABINDS: @[%a@];@!FUNBINDS: @[%a@]" 
    d_ldbinds llds
    d_lcfbinds lcfs

let d_slocs () ls =
  Pretty.seq ~sep:(Pretty.text ",") ~doit:(Sloc.d_sloc()) ~elements:ls

let slocs_of_bindings (lds, lfs) = 
  (List.map fst lds ++ List.map fst lfs) 
  |> Misc.sort_and_compact

let d_bindings () x  = x |> slocs_of_bindings |> d_slocs ()


let is_live_name livem n =
  match FA.base_of_name n with
  | None    -> true
  | Some bn -> if YM.mem bn livem then n = YM.find bn livem else true

let env_of_cilenv (_, vnv, _) = 
  FA.builtinm
  |> YM.fold (fun n rct env -> YM.add n (Ct.reft_of_refctype rct) env) vnv
  |> canon_env

let make_wfs ((_,_,livem) as cenv) sto rct _ =
  let r   = rct |> Ct.reft_of_refctype |> canon_reft in
  let env = cenv 
            |> env_of_cilenv
            |> YM.filter (fun n _ -> n |> Sy.to_string |> Co.is_cil_tempvar |> not)
            |> (if !Co.prune_live then YM.filter (fun n _ -> is_live_name livem n) else id)
  in [C.make_filtered_wf env r None (filter_store_derefs cenv sto rct)]
(* >> F.printf "\n make_wfs: \n @[%a@]" (Misc.pprint_many true "\n" (C.print_wf None)) 
*)

let make_wfs_effect env sto l eptr =
  let env = l
         |> RCt.Store.Data.find_or_empty sto
         |> sloc_binds_of_refldesc l
         |> List.filter (not <.> Ix.is_periodic <.> snd)
         |> List.map fst
         |> ce_adds env in
    with_effects_in_env env (fun env -> make_wfs env sto eptr ())

let make_wfs_effectset env sto effs =
     effs
  |> Ct.EffectSet.maplisti (make_wfs_effect env sto)
  |> List.concat

let rec make_wfs_refstore env full_sto sto tag =
  RCt.Store.Function.fold_locs (fun l rft ws -> make_wfs_fn env rft tag ++ ws) [] sto ++
    RCt.Store.Data.fold_locs begin fun l rd ws ->
      let ncrs = sloc_binds_of_refldesc l rd in
      let env' = ncrs |> List.filter (not <.> Ix.is_periodic <.> snd) 
                      |> List.map fst
                      |> ce_adds env in
      let env_addr = ce_adds env' [(vv_addr, t_addr l)] in
      let ws1  = Misc.flap
                   (fun ((_,cr),i) -> make_wfs (M.choose (Ix.is_periodic i) env_addr env') full_sto cr tag)
                   ncrs in
        ws1 ++ ws
    end [] sto

and make_wfs_fn cenv rft tag =
  let args  = List.map (Misc.app_fst Sy.of_string) rft.Ct.args in
  let env'  = ce_adds cenv args in
  let retws = make_wfs env' rft.Ct.sto_out rft.Ct.ret tag in
  let argws = Misc.flap (fun (_, rct) -> make_wfs env' rft.Ct.sto_in rct tag) args in
  let inws  = make_wfs_refstore env' rft.Ct.sto_in rft.Ct.sto_in tag in
  let outws = make_wfs_refstore env' rft.Ct.sto_out rft.Ct.sto_out tag in
  let effws = make_wfs_effectset env' rft.Ct.sto_out rft.Ct.effects in
  Misc.tr_rev_flatten [retws ; argws ; inws ; outws; effws]

let make_dep pol xo yo =
  (xo, yo) |> Misc.map_pair (Misc.maybe_map CilTag.tag_of_t)
           |> Misc.uncurry (C.make_dep pol)

let make_cs cenv p rct1 rct2 tago tag =
  let env    = env_of_cilenv cenv in
  let r1, r2 = Misc.map_pair (Ct.reft_of_refctype <+> canon_reft) (rct1, rct2) in
  let r1     = if !Co.simplify_t then strengthen_reft env r1 else r1 in
  let cs     = [C.make_t env p r1 r2 None (CilTag.tag_of_t tag)] in
  let ds     = [] (* add_deps tago tag *) in
  cs, ds

let make_cs_assert_disjoint env p cr1 cr2 tago tag =
  make_cs env p (meet_refctype cr1 cr2) (t_false_refctype cr2) tago tag

let with_refldesc_ncrs_env_subs env (sloc1, rd1) (sloc2, rd2) f =
  let ncrs1  = sloc_binds_of_refldesc sloc1 rd1 in
  let ncrs2  = sloc_binds_of_refldesc sloc2 rd2 in
  let env    = ncrs1
            |> List.filter (not <.> Index.is_periodic <.> snd)
            |> List.map fst
            |> ce_adds env in
  let ncrs12 = Misc.join snd ncrs1 ncrs2
            |> List.map begin fun ((x,i), (y,_)) ->
                 (x,y,i)
               end in  
(*  let _      = asserts ((* TBD: HACK for malloc polymorphism *) ncrs1 = [] 
                       || List.length ncrs12 = List.length ncrs2) "make_cs_refldesc" in *)
  let subs   = ncrs12
            |> List.filter (not <.> Index.is_periodic <.> thd3)
            |> List.map (fun ((n1,_), (n2,_), _) -> (n2, n1)) in
    f ncrs12 env subs

let make_cs_assert_effects_disjoint env p eptr1 eptr2 tago tag =
  with_effects_in_env env begin fun env ->
        ED.getEffects ()
    |>  M.dup
    |>  M.uncurry M.cross_product
    |>  M.negfilter (M.uncurry ED.effectsCommute)
    |>: begin fun (eff1, eff2) ->
        let sub1, sub2 = (eff1, eff2)
                      |> M.map_pair (fun n -> [(ED.nameOfEffect n, e_cil_effect_true)]) in
          make_cs_assert_disjoint env p
            (t_subs_exps sub1 eptr1)
            (t_subs_exps sub2 eptr2)
            tago tag
        end
    |>  M.splitflatten
  end

let make_cs_assert_effectsets_disjoint env p sto effs1 effs2 tago tag =
     effs1
  |> ES.domain
  |> List.map begin fun l ->
       let eff1, eff2 = M.map_pair (M.flip ES.find l) (effs1, effs2) in
         if RCt.Store.Data.mem sto l then
           let ld = RCt.Store.Data.find sto l in
             with_refldesc_ncrs_env_subs env (l, ld) (l, ld) begin fun _ env _ ->
               make_cs_assert_effects_disjoint env p eff1 eff2 tago tag
             end
         else ([], [])
     end
  |> M.splitflatten

let make_cs_effect_weaken_type env p sto erct eff eptr tago tag =
  let cl = erct |> RCt.CType.sloc |> M.maybe in
  let al = Sloc.canonical cl in
    with_effects_in_env env begin fun env ->
      if RCt.Store.Data.mem sto cl then
        let lhsld = (cl, RCt.Store.Data.find sto cl) in
        let rhsld = (al, RCt.Store.Data.find sto al) in
          with_refldesc_ncrs_env_subs env lhsld rhsld begin fun _ env subs ->
            make_cs env p erct (t_subs_names subs eptr) tago tag
          end
      else make_cs env p erct eptr tago tag
    end

let make_cs_effect_weaken_var env p sto v eff eptr tago tag =
  make_cs_effect_weaken_type env p sto (t_singleton_effect env v eff) eff eptr tago tag

let make_cs_data_effect env p sld1 sld2 eptr1 eptr2 tago tag =
  with_effects_in_env env begin fun env ->
    with_refldesc_ncrs_env_subs env sld1 sld2 begin fun _ env subs ->
      make_cs env p eptr1 (t_subs_names subs eptr2) tago tag
    end
  end

let make_cs_function_effect env p eptr1 eptr2 tago tag =
  with_effects_in_env env (fun env -> make_cs env p eptr1 eptr2 tago tag)

let make_cs_subtyping_bind_pairs polarity binds1 binds2 f =
  let dom = List.map fst (if polarity then binds2 else binds1) in
       (binds1, binds2)
    |> Misc.map_pair (List.filter (fun (sloc, _) -> List.mem sloc dom))
    |> Misc.uncurry (Misc.full_join fst)
    |> List.map f
    |> Misc.splitflatten

let make_cs_effectset_binds polarity env p (sldes1, sfnes1) (sldes2, sfnes2) tago tag =
  make_cs_subtyping_bind_pairs polarity sldes1 sldes2 begin fun ((l1, (ld1, eff1)), (l2, (ld2, eff2))) ->
    make_cs_data_effect env p (l1, ld1) (l2, ld2) eff1 eff2 tago tag
  end +++
  make_cs_subtyping_bind_pairs polarity sfnes1 sfnes2 begin fun ((l1, (_, eff1)), (l2, (_, eff2))) ->
    make_cs_function_effect env p eff1 eff2 tago tag
  end

let make_cs_effectset env p sto1 sto2 effs1 effs2 tago tag =
  make_cs_effectset_binds true env p
    (RCt.Store.join_effects (RCt.Store.abstract sto1) effs1)
    (RCt.Store.join_effects (RCt.Store.abstract sto2) effs2)
    tago
    tag

let make_cs_refldesc env p sld1 sld2 tago tag =
  with_refldesc_ncrs_env_subs env sld1 sld2 begin fun ncrs env subs ->
    let env_addr = ce_adds env [(vv_addr, sld1 |> fst |> Sloc.canonical |> t_addr)] in
     Misc.map begin fun ((n1, cr1), (_, cr2), i) -> 
       let lhs = if Index.is_periodic i then cr1 else t_name env n1 in
       let rhs = t_subs_names subs cr2 in
       let env = if Ix.is_periodic i then env_addr else env in
         make_cs env p lhs rhs tago tag 
     end ncrs
     |> Misc.splitflatten
  end 

(* API *)
let make_cs cenv p rct1 rct2 tago tag loc =
(*  let _ = Pretty.printf "make_cs: rct1 = %a, rct2 = %a \n" d_refctype rct1 d_refctype rct2 in
 *) try make_cs cenv p rct1 rct2 tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs" in 
    assert false

let make_cs_assert cenv p passert tago tag loc =
  let vv = Ct.scalar_ctype |> sort_of_prectype |> Sy.value_variable in
    make_cs cenv p (t_true Ct.scalar_ctype) (t_pred Ct.scalar_ctype vv passert) tago tag loc

(* API *)
let make_cs_assert_effectsets_disjoint env p sto effs1 effs2 tago tag loc =
  try make_cs_assert_effectsets_disjoint env p sto effs1 effs2 tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_assert_effectsets_disjoint fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_assert_effectsets_disjoint" in
    assert false

(* API *)
let make_cs_tuple env grd lsubs subs cr1s cr2s tago tag loc =
  Misc.map2 begin fun cr1 cr2 ->
    make_cs env grd cr1 (rename_refctype lsubs subs cr2) tago tag loc
  end cr1s cr2s 
  |> Misc.splitflatten


let rec make_cs_refstore_aux subf env p st1 st2 polarity tago tag loc =
 (* let _  = Pretty.printf "make_cs_refstore: pol = %b, st1 = %a, st2 = %a, loc = %a \n"
           polarity Ct.d_prestore_addrs st1 Ct.d_prestore_addrs st2 Cil.d_loc loc in
  let _  = Pretty.printf "st1 = %a \n" d_refstore st1 in
  let _  = Pretty.printf "st2 = %a \n" d_refstore st2 in  
*)  make_cs_refstore_binds_aux subf
    env p (RCt.Store.bindings st1) (RCt.Store.bindings st2) polarity tago tag loc

and make_cs_refstore_binds_aux subf env p (slds1, sfuns1) (slds2, sfuns2) polarity tago tag loc =
  Misc.splitflatten
    [make_cs_refstore_data_binds env p slds1 slds2 polarity tago tag loc;
     make_cs_refstore_fun_binds subf env p sfuns1 sfuns2 polarity tago tag loc]

and make_cs_refstore_data_binds env p slds1 slds2 polarity tago tag loc =
  make_cs_subtyping_bind_pairs polarity slds1 slds2 begin fun (sld1, sld2) ->
    make_cs_refldesc env p sld1 sld2 tago tag
  end

and make_cs_refstore_fun_binds subf env p sfuns1 sfuns2 polarity tago tag loc =
  make_cs_subtyping_bind_pairs polarity sfuns1 sfuns2 begin fun ((_, fun1), (_, fun2)) ->
    let cf1, cf2 = M.map_pair Ct.cfun_of_refcfun (fun1, fun2) in
      if Ct.I.CFun.same_shape cf1 cf2 then
        subf env p fun1 fun2 tag loc
      else Errormsg.s <|
          Cil.error "Cannot subtype differently-shaped functions:@!%a@!<:@!%a@!@!"
            Ct.I.CFun.d_cfun cf1 Ct.I.CFun.d_cfun cf2
  end

and make_cs_refcfun_aux subf env p ((it, it'), (hi, hi'), (ocr, ocr'), (ho, ho')) tag loc =
  let env         = it' |> List.map (Misc.app_fst FA.name_of_string)
                        |> ce_adds env in
  let ircs, ircs' = Misc.map_pair (List.map snd) (it, it') in
  (* contravariant inputs *)
      (make_cs_tuple env p [] [] ircs' ircs None tag loc)  
  +++ (make_cs_refstore_aux subf env p hi' hi true None tag loc) 
  (* covariant outputs *)
  +++ (make_cs env p ocr ocr' None tag loc)
  +++ (make_cs_refstore_aux subf env p ho ho' true None tag loc)

let make_cs_refcfun_components rf rf' =
  let rf, rf'     = RCt.CFun.normalize_names rf rf' subs_refctype subs_refctype in
  let it, it'     = Misc.map_pair Ct.args_of_refcfun (rf, rf') in
  let ocr, ocr'   = Misc.map_pair Ct.ret_of_refcfun (rf, rf') in
  let hi, ho      = Ct.stores_of_refcfun rf in
  let hi',ho'     = Ct.stores_of_refcfun rf' in
    ((it, it'), (hi, hi'), (ocr, ocr'), (ho, ho'))

let rec make_cs_refcfun env p rf rf' tag loc =
  make_cs_refcfun_aux make_cs_refcfun
    env p (make_cs_refcfun_components rf rf') tag loc

(* API *)
let make_cs_refstore env p st1 st2 polarity tago tag loc =
  try make_cs_refstore_aux make_cs_refcfun env p st1 st2 polarity tago tag loc with ex ->
    let _ = Cil.errorLoc loc "make_cs_refstore fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_refstore" in 
    assert false

(* API *)
let make_cs_refstore_binds env p binds1 binds2 polarity tago tag loc =
  try make_cs_refstore_binds_aux make_cs_refcfun env p binds1 binds2 polarity tago tag loc with ex ->
    let _ = Cil.errorLoc loc "make_cs_refstore_binds fails with: %s" (Printexc.to_string ex) in
    let _ = Pretty.printf "make_cs_refstore_binds: pol = %b, binds1 = %a, binds2 = %a, loc = %a \n"
            polarity d_bindings binds1 d_bindings binds2 Cil.d_loc loc in
    let _ = asserti false "make_cs_refstore_binds" in 
    assert false


(* API *)
let make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) tago tag loc =
  try make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_refldesc fails with: %s" (Printexc.to_string ex) in 
    let _ = asserti false "make_cs_refldesc" in 
    assert false

(* API *)
let make_cs_effect_weaken_var env p sto v eff eptr tago tag loc =
  try make_cs_effect_weaken_var env p sto v eff eptr tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_effect_weaken_var fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_effect_weaken_var" in
    assert false

(* API *)
let make_cs_effect_weaken_type env p sto erct eff eptr tago tag loc =
  try make_cs_effect_weaken_type env p sto erct eff eptr tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_effect_weaken_type fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_effect_weaken_type" in
    assert false

(* API *)
let make_cs_effectset env p sto1 sto2 effs1 effs2 tago tag loc =
  try make_cs_effectset env p sto1 sto2 effs1 effs2 tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_effectset fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_effectset" in
    assert false

(* API *)
let make_cs_effectset_binds polarity env p binds1 binds2 tago tag loc =
  try make_cs_effectset_binds polarity env p binds1 binds2 tago tag with ex->
    let _ = Cil.errorLoc loc "make_cs_effectset fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_effectset" in
    assert false


(* API *) 
let make_cs_refcfun gnv p rf rf' tag loc =
  try make_cs_refcfun gnv p rf rf' tag loc with ex ->
    let _ = Cil.errorLoc loc "make_cs_refcfun fails with: %s" (Printexc.to_string ex) in 
    let _ = asserti false "make_cs_refcfun" in 
    assert false

let new_block_reftype = t_zero_refctype (* t_true_refctype *)


(* API: TBD: UGLY *)
let extend_world ssto sloc cloc newloc strengthen loc tag (env, sto, tago) =
  let ld    = sloc |> Ct.refstore_get ssto |> strengthen in
  let binds = binds_of_refldesc sloc ld 
              |> (Misc.choose newloc (List.map (Misc.app_snd new_block_reftype)) id) in 
  let subs  = List.map (fun (n,_) -> (n, FA.name_fresh ())) binds in
  let env'  = Misc.map2 (fun (_, cr) (_, n') -> (n', cr)) binds subs
              |> Misc.map (Misc.app_snd (t_subs_names subs))
              |> ce_adds env in
  let _, im = Misc.fold_lefti (fun i im (_,n') -> IM.add i n' im) IM.empty subs in
  let ld'   = ld
           |> RCt.LDesc.mapn begin fun i ix rfld ->
                let fnl = RCt.Field.get_finality rfld in
                  if IM.mem i im then IM.find i im |> t_name env' |> RCt.Field.create fnl Ct.dummy_fieldinfo else
                    match ix with
                      | Ix.IInt _ -> assertf "missing binding!"
                      | _         -> RCt.Field.map_type (t_subs_names subs) rfld
              end in
  let cs    = if not newloc then [] else
                RCt.LDesc.foldn begin fun i cs ix rfld ->
                  match ix with
                  | Ix.ICClass _ ->
                      let rct = RCt.Field.type_of rfld in
                      let lhs = rct
                             |> new_block_reftype
                             |> strengthen_refctype
                                 begin fun rct ->
                                   let vv, p = Sc.pred_of_index_ref ix in
                                     [C.Conc (P.subst p vv (A.eVar vv_addr))]
                                 end in
                      let rhs = t_subs_names subs rct in
                      let env' = ce_adds env' [(vv_addr, t_addr sloc)] in
                      let cs' = fst <| make_cs env' A.pTrue lhs rhs None tag loc in
                      cs' ++ cs
                  | _ -> cs
                end [] ld in
  let sto'  = Ct.refstore_set sto cloc ld' in
  (env', sto', tago), cs

let strengthen_type_with_deref ptrexp off ty =
  strengthen_refctype (fun ct -> ra_deref ct ptrexp off) ty

let strengthen_final_field ffs ptrname i fld =
  let ptr_base = ptrname |> Sy.of_string |> A.eVar |> FA.eApp_bbegin in
    match i with
      | Ix.ICClass _ | Ix.IBot -> fld
      | Ix.IInt n              ->
          if Ix.IndexSet.mem i ffs then
            fld
            |> RCt.Field.map_type (strengthen_type_with_deref ptr_base n)
            |> M.flip RCt.Field.set_finality Ct.Final
          else
            fld

let finalized_name = "FINAL" |> Misc.mk_string_factory |> fst

let refstore_strengthen_addr loc env sto ffm ptrname addr =
  let cl, i = Ct.addr_of_refctype loc addr in
  let _     = assert (not (Sloc.is_abstract cl)) in
  let ffs   = Sloc.SlocMap.find cl ffm in
    if Ix.IndexSet.mem i ffs then
      let ld  = RCt.Store.Data.find sto cl in
      let fld = ld |> RCt.LDesc.find i |> List.hd |> snd in
      let ld  = RCt.LDesc.remove i ld in
      let sct = fld |> strengthen_final_field ffs ptrname i |> RCt.Field.type_of in
      let fn  = () |> finalized_name |> Sy.of_string in
      let env = ce_adds env [(fn, sct)] in
      let fld = t_equal (Ct.ctype_of_refctype sct) fn |> RCt.Field.create Ct.Final Ct.dummy_fieldinfo in
      let ld  = RCt.LDesc.add i fld ld in
        (env, RCt.Store.Data.add sto cl ld)
    else
      (env, sto)

(* API: shady hack -- remove when "Solve.force" properly implemented 
let annot_binds () = 
  let cf = fun _ -> None in
  !annotr
  |> Misc.map_partial begin function 
       | TVar (n, env, cr) -> Some (name_of_string n, (env_of_cilenv cf env, reft_of_refctype cr))
       | _                 -> None
     end 
  |> Ast.Symbol.sm_of_list

  *)


