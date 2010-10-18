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
module Su = Ast.Subst
module FI = FixInterface
module SM = Misc.StringMap
module YM = Ast.Symbol.SMap
module ST = Ssa_transform
module Ct = Ctypes
module Ix = Ct.Index
module Co = Constants
module P  = Ast.Predicate 
module Q  = Ast.Qualifier

module Ci = Consindex
module E  = Errormsg

open Misc.Ops

type scalar_const = Offset of int | UpperBound of int | Periodic of int * int

(***************************************************************************)
(******************** Meta Qualifiers for Scalar Invariants ****************)
(***************************************************************************)

let index_of_ctype ct =
  match Ctypes.I.CType.refinements_of_t ct with
  | [ix] -> ix
  | _    -> assertf "Scalar.index_of_ctype"

let value_var       = Ast.Symbol.value_variable Ast.Sort.t_int
let const_var       = Ast.Symbol.mk_wild ()
let param_var       = Ast.Symbol.mk_wild ()

(* v = c *)
let p_v_eq_c        = Ast.pEqual (Ast.eVar value_var, Ast.eVar const_var)
(* v < c *)
let p_v_lt_c        = Ast.pAtom (Ast.eVar value_var, Ast.Lt, Ast.eVar const_var)
(* v >= c *)
let p_v_ge_c        = Ast.pAtom (Ast.eVar value_var, Ast.Ge, Ast.eVar const_var)

(* v = _ + c *)
let p_v_eq_x_plus_c = Ast.pEqual (Ast.eVar value_var, Ast.eBin (Ast.eVar param_var, Ast.Plus, Ast.eVar const_var))
(* v < _ + c *)
let p_v_lt_x_plus_c = Ast.pAtom (Ast.eVar value_var, Ast.Lt, Ast.eBin (Ast.eVar param_var, Ast.Plus, Ast.eVar const_var))



let quals_of_pred p = List.map (fun t -> Q.create value_var t p) [Ast.Sort.t_int]


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
  | Ct.PosB k -> [UpperBound (n + m*k)]
  | _             -> []

let scalar_consts_of_index = function
  | Ix.IBot            -> []
  | Ix.IInt n          -> [Offset n] 
  | Ix.ISeq (n, m, po) -> [Offset n;  Periodic (n, m)] ++ (scalar_consts_of_polarity n m po)
  
let preds_of_scalar_const = function
  | Offset c ->
      [p_v_eq_c; p_v_ge_c; p_v_eq_x_plus_c] 
      |>: (Misc.flip Ast.substs_pred) (Ast.Subst.of_list [const_var, Ast.eInt c])
      
  | UpperBound c ->
      [p_v_lt_c; p_v_lt_x_plus_c] 
      |>: (Misc.flip Ast.substs_pred) (Ast.Subst.of_list [const_var, Ast.eInt c])
  
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

let scalar_quals_of_file cil = 
  cil 
  |> type_decs_of_file
  |> Misc.map (Misc.uncurry Genspec.spec_of_type)
  |> Misc.flap (fun (ct, st) -> [index_of_ctype ct] ++ Ctypes.I.Store.indices_of_t st)
  |> Misc.flap scalar_consts_of_index
  |> Misc.sort_and_compact
  |> Misc.flap preds_of_scalar_const
  |> Misc.flap quals_of_pred
  |> (++) (FI.quals_of_file (Co.get_lib_squals ()))
  >> dump_quals_to_file (!Co.liquidc_file_prefix ^ ".squals")

(***************************************************************************)
(********************** Convert Predicates To Indices **********************)
(***************************************************************************)

(* 
let unify_pred p q =
  Ast.unify_pred p q
  >> Misc.maybe_iter (fun su -> ignore <| Format.printf "unify_pred: p is %a, q is %a, subst = %a \n" 
                                          P.print p P.print q Su.print su)

let ppp = Ast.substs_pred p_v_eq_c (Su.of_list [const_var, Ast.eInt 0])
let _   = unify_pred p_v_eq_c ppp 
*)

let const_of_subst su =
  su |> Ast.Subst.to_list
     |> Misc.do_catch (Format.sprintf "Scalar.const_of_subst") (List.assoc const_var)
     |> (function Ast.Con (Ast.Constant.Int i), _  -> Some i | _ -> None)

let const_of_preds p f v ps =
  let p = [value_var, Ast.eVar v] |> Ast.Subst.of_list |> Ast.substs_pred p in
  ps |> Misc.map_partial (Ast.unify_pred p)
     |> Misc.map_partial const_of_subst
     |> f 

let indexo_of_preds_iint =
  const_of_preds p_v_eq_c begin function 
    | c::cs -> Some (Ix.IInt (List.fold_left min c cs))
    | []    -> None 
  end

let indexo_of_preds_lowerbound =
  const_of_preds p_v_ge_c begin function 
    | c::cs -> Some (Ix.ISeq (List.fold_left max c cs, 1, Ct.Pos))
    | []    -> None 
  end

(*
let indexo_of_preds_iint v ps =
  let p_v_eq_c = [value_var, Ast.eVar v] |> Ast.Subst.of_list |> Ast.substs_pred p_v_eq_c in
  ps |> Misc.map_partial (Ast.unify_pred p_v_eq_c)
     |> Misc.map_partial const_of_subst
     |> (function [] -> None | c::cs -> Some (Ix.IInt (List.fold_left min c cs)))
*)


(*
try indexo_of_preds_iint v ps with ex ->
  try indexo_of_preds_iint v ps with ex ->
    (Printf.printf "indexo_of_preds_iseq v = %s, ps =%s" 
    (Sy.to_string v)
    (P.to_string (Ast.pAnd ps));
    raise ex)
*)

let indexo_of_preds_iseqb v ps = 
  None (* TODO *)

let indexo_of_preds_iseq  v ps = 
  None (* TODO *) 

let index_of_pred v = 
  let v = FI.name_of_varinfo v in
  function
  | Ast.And ps, _ ->
    [ indexo_of_preds_iint v
    ; indexo_of_preds_iseqb v
    ; indexo_of_preds_iseq v 
    ; indexo_of_preds_lowerbound v 
    ]
    |> Misc.maybe_chain ps Ix.top
  | _ -> assertf "Scalar.index_of_pred"

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
  scalar_quals_of_file cil 
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

let close_locals locals vm : Ix.t VM.t =
  locals 
  |> List.filter (fun v -> not (VM.mem v vm)) 
  |> List.fold_left (fun vm v -> if VM.mem v vm then vm else VM.add v Ix.top vm) vm

let close scim spec sim : Ix.t VM.t SM.t =
  SM.mapi begin fun fn vm ->
    let fdec = (SM.find fn scim).ST.fdec in
    let args = ix_binds_of_spec spec fn in
    vm |> close_formals args fdec.Cil.sformals 
       |> close_locals fdec.Cil.slocals
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
  ix = ix'

let check_scalar shm sim = 
  let oc  = open_out (!Co.liquidc_file_prefix ^ ".scalarlog") in
  let ppf = Format.formatter_of_out_channel oc in
  (SM.fold begin fun fn { Shape.vtyps = vcts } errs ->
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
  check_scalar shm sim
  >> (List.iter (fun e -> E.warn "%a \n" d_scalar_error e |> ignore))
  >> (fun _ -> Errormsg.log "DONE: scalar testing \n")
  |> (function [] -> exit 0 | _ -> exit 1)
