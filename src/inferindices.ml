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

module M   = Misc
module P   = Pretty
module E   = Errormsg
module C   = Cil
module CM  = CilMisc
module VM  = CM.VarMap
module S   = Sloc
module SLM = S.SlocMap
module ST  = Ssa_transform
module Cs  = Constants
module FI  = FixInterface
module FC  = FixConstraint
module SI  = ShapeInfra

open Ctypes
open M.Ops

(******************************************************************************)
(****************************** Index Constraints *****************************)
(******************************************************************************)

type indexvar = int

let d_indexvar () (iv: indexvar): P.doc =
  P.text <| "?i" ^ string_of_int iv

let (fresh_indexvar, reset_fresh_indexvars) = M.mk_int_factory ()

let with_fresh_indexvar (f: indexvar -> 'a): 'a =
  fresh_indexvar () |> f

type indexexp =
  | IEConst of Index.t
  | IEVar of indexvar
  | IEPlus of indexexp * (* RHS scale: *) int * indexexp
  | IEMinus of indexexp * (* RHS scale: *) int * indexexp
  | IEMult of indexexp * indexexp
  | IEDiv of indexexp * indexexp
  | IEUnsign of indexexp

let rec d_indexexp (): indexexp -> P.doc = function
  | IEConst i                 -> Index.d_index () i
  | IEVar iv                  -> d_indexvar () iv
  | IEPlus (ie1, scale, ie2)  -> P.dprintf "%a + %d * %a" d_indexexp ie1 scale d_indexexp ie2
  | IEMinus (ie1, scale, ie2) -> P.dprintf "%a - %d * %a" d_indexexp ie1 scale d_indexexp ie2
  | IEMult (ie1, ie2)         -> P.dprintf "%a * %a" d_indexexp ie1 d_indexexp ie2
  | IEDiv (ie1, ie2)          -> P.dprintf "%a / %a" d_indexexp ie1 d_indexexp ie2
  | IEUnsign ie               -> P.dprintf "(unsigned) %a" d_indexexp ie

let indexexp_vars_aux: indexexp -> indexexp list * indexvar list = function
  | IEPlus (ie1, _, ie2)
  | IEMinus (ie1, _, ie2)
  | IEMult (ie1, ie2)
  | IEDiv (ie1, ie2) -> ([ie1; ie2], [])
  | IEConst _        -> ([], [])
  | IEVar iv         -> ([], [iv])
  | IEUnsign ie      -> ([ie], [])

let indexexp_vars (ie: indexexp): indexvar list =
  M.expand indexexp_vars_aux [ie] []

module IndexSol =
  Misc.MapWithDefault(struct
                        type t      = indexvar
                        let compare = compare

                        type v      = Index.t
                        let default = Index.IBot
                      end)

type indexsol = Index.t IndexSol.t

module ISPrinter = P.MakeMapPrinter(IndexSol)

let d_indexsol =
  ISPrinter.d_map "\n" d_indexvar Index.d_index

let rec indexexp_apply is = function
  | IEConst i             -> i
  | IEVar iv              -> IndexSol.find iv is
  | IEUnsign ie           -> Index.unsign (indexexp_apply is ie)
  | IEPlus (ie1, x, ie2)  -> Index.plus (indexexp_apply is ie1) (Index.scale x <| indexexp_apply is ie2)
  | IEMinus (ie1, x, ie2) -> Index.minus (indexexp_apply is ie1) (Index.scale x <| indexexp_apply is ie2)
  | IEMult (ie1, ie2)     -> Index.mult (indexexp_apply is ie1) (indexexp_apply is ie2)
  | IEDiv (ie1, ie2)      -> Index.div (indexexp_apply is ie1) (indexexp_apply is ie2)

let refine_index (is: indexsol) (ie: indexexp) (iv: indexvar): indexsol =
  IndexSol.add iv (Index.lub (indexexp_apply is ie) (IndexSol.find iv is)) is

let bounded_refine_index (is: indexsol) (ie: indexexp) (iv: indexvar) (ibound: Index.t): indexsol =
  let is = refine_index is ie iv in
    if Index.is_subindex (IndexSol.find iv is) ibound then is else IndexSol.add iv ibound is

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

type itypevar = indexexp prectype

type ifunvar = indexexp precfun

let d_itypevar: unit -> itypevar -> P.doc =
  d_prectype d_indexexp

let d_ifunvar: unit -> ifunvar -> P.doc =
  d_precfun d_indexexp

let itypevar_indexvars: itypevar -> indexvar list = function
  | CTInt (_, ie) | CTRef (_, ie) -> indexexp_vars ie

let itypevar_of_ctype: ctype -> itypevar = function
  | CTInt (n, i) -> CTInt (n, IEConst i)
  | CTRef (s, i) -> CTRef (s, IEConst i)

let ifunvar_of_cfun (cf: cfun): ifunvar =
  precfun_map itypevar_of_ctype cf

let fresh_itypevar (t: C.typ): itypevar =
  match C.unrollType t with
    | C.TInt (ik, _)        -> CTInt (C.bytesSizeOfInt ik, IEVar (fresh_indexvar ()))
    | C.TEnum (ei, _)       -> CTInt (C.bytesSizeOfInt ei.C.ekind, IEVar (fresh_indexvar ()))
    | C.TFloat _            -> CTInt (CM.typ_width t, IEConst Index.top)
    | C.TVoid _             -> itypevar_of_ctype void_ctype
    | C.TPtr _ | C.TArray _ -> CTRef (S.none, IEVar (fresh_indexvar ()))
    | t                     -> E.s <| C.bug "Unimplemented fresh_itypevar: %a@!@!" C.d_type t

let itypevar_apply (is: indexsol) (itv: itypevar): ctype =
  prectype_map (indexexp_apply is) itv

let is_subitypevar (is: indexsol) (itv1: itypevar) (itv2: itypevar): bool =
  match itv1, itv2 with
    | CTInt (n1, ie1), CTInt (n2, ie2) when n1 = n2 -> Index.is_subindex (indexexp_apply is ie1) (indexexp_apply is ie2)
    | CTRef (_, ie1), CTRef (_, ie2)                -> Index.is_subindex (indexexp_apply is ie1) (indexexp_apply is ie2)
    | _                                             -> false

type boundfun = string * (ctype -> ctype * FI.refctype)

let ctype_of_bound ((_, fbound): boundfun) (ctv: ctype): ctype =
  ctv |> fbound |> fst

let bound_nonneg (ct: ctype): ctype * FI.refctype =
  match ct with
    | CTInt (w, Index.ISeq (n, p, PosNeg)) ->
        let vv   = Ast.Symbol.value_variable Ast.Sort.t_int in
        let pred = Ast.pAtom (Ast.eVar vv, Ast.Ge, Ast.eCon (Ast.Constant.Int 0)) in
        (CTInt (w, Index.ISeq (n, p, Pos)), FI.t_pred ct vv pred)
    | _ -> (ct, FI.t_true ct)

type 'a pretypecstrdesc =
  | ISubtype of 'a * 'a
  | IDSubtype of 'a * 'a * C.varinfo * boundfun

let d_pretypecstrdesc (d_type: unit -> 'a -> P.doc) (): 'a pretypecstrdesc -> P.doc = function
  | ISubtype (itv1, itv2)                   -> P.dprintf "%a <: %a" d_type itv1 d_type itv2
  | IDSubtype (itv1, itv2, v, (fbdesc, fb)) -> P.dprintf "%a <: %a :: %a <: %s (%a)" d_type itv1 CM.d_var v d_type itv2 fbdesc d_type itv1

let pretypecstrdesc_apply (is: indexsol) (app_type: indexsol -> 'a -> 'b): 'a pretypecstrdesc -> 'b pretypecstrdesc = function
  | ISubtype (t1, t2)         -> ISubtype (app_type is t1, app_type is t2)
  | IDSubtype (t1, t2, v, bf) -> IDSubtype (app_type is t1, app_type is t2, v, bf)

type ctypecstrdesc = ctype pretypecstrdesc

let d_ctypecstrdesc () (ctcd: ctypecstrdesc): P.doc =
  d_pretypecstrdesc d_ctype () ctcd

type itypevarcstrdesc = itypevar pretypecstrdesc

let d_itypevarcstrdesc () (itcd: itypevarcstrdesc): P.doc =
  d_pretypecstrdesc d_itypevar () itcd

let itypevarcstrdesc_apply (is: indexsol) (itcd: itypevarcstrdesc): ctypecstrdesc =
  pretypecstrdesc_apply is itypevar_apply itcd

type 'a pretypecstr = {itcid: int; itcdesc: 'a pretypecstrdesc; itcloc: C.location}

let d_pretypecstr (d_type: unit -> 'a -> P.doc) () ({itcid = id; itcdesc = pitcd; itcloc = loc}: 'a pretypecstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc (d_pretypecstrdesc d_type) pitcd

let pretypecstr_apply (is: indexsol) (app_type: indexsol -> 'a -> 'b) (pitc: 'a pretypecstr): 'b pretypecstr =
  {pitc with itcdesc = pretypecstrdesc_apply is app_type pitc.itcdesc}

type ctypecstr = ctype pretypecstr

let d_ctypecstr () (ctc: ctypecstr): P.doc =
  d_pretypecstr d_ctype () ctc

type itypevarcstr = itypevar pretypecstr

let d_itypevarcstr () (itc: itypevarcstr): P.doc =
  d_pretypecstr d_itypevar () itc

let itypevarcstr_apply (is: indexsol) (itc: itypevarcstr): ctypecstr =
  pretypecstr_apply is itypevar_apply itc

let (fresh_itypevarcstrid, reset_fresh_itypevarcstrids) = M.mk_int_factory ()

let mk_isubtypecstr (itv1: itypevar) (itv2: itypevar): itypevarcstr =
  {itcid = fresh_itypevarcstrid (); itcloc = !C.currentLoc; itcdesc = ISubtype (itv1, itv2)}

let mk_idsubtypecstr (itv1: itypevar) (itv2: itypevar) (v: C.varinfo) (bf: boundfun): itypevarcstr =
  {itcid = fresh_itypevarcstrid (); itcloc = !C.currentLoc; itcdesc = IDSubtype (itv1, itv2, v, bf)}

let itypevarcstr_id ({itcid = id}: itypevarcstr): int =
  id

let itypevarcstr_rhs_var (itc: itypevarcstr): indexvar option =
  match itc.itcdesc with
    | ISubtype (_, itv) | IDSubtype (_, itv, _, _) ->
        match itypevar_indexvars itv with
          | []   -> None
          | [iv] -> Some iv
          | _    -> halt <| C.errorLoc itc.itcloc "Ill-formed constraint: %a\n" d_itypevarcstr itc

let itypevarcstr_sat (is: indexsol) (itc: itypevarcstr): bool =
  match itc.itcdesc with
    | ISubtype (itv1, itv2)         -> is_subitypevar is itv1 itv2
    | IDSubtype (itv1, itv2, _, bf) ->
        let itvbound = itv1 |> itypevar_apply is |> ctype_of_bound bf |> itypevar_of_ctype in
             is_subitypevar is itv2 itvbound
          && ((is_subitypevar is itv1 itvbound && is_subitypevar is itv1 itv2)
              || (not (is_subitypevar is itv1 itvbound) && is_subitypevar is itvbound itv2))

let fail_constraint (is: indexsol) (itc: itypevarcstr): 'a =
  halt <| C.errorLoc itc.itcloc "Failed index constraint %a\n\n" d_ctypecstr (itypevarcstr_apply is itc)

let refine_itypevarcstr (is: indexsol) (itc: itypevarcstr): indexsol =
  match itc.itcdesc with
    | ISubtype (itv1, itv2) ->
        begin match itv1, itv2 with
          | CTInt (n1, ie), CTInt (n2, IEVar iv) when n1 = n2 -> refine_index is ie iv
          | CTRef (_, ie), CTRef (_, IEVar iv)                -> refine_index is ie iv
          | _                                                 -> fail_constraint is itc
        end
    | IDSubtype (itv1, itv2, _, bf) ->
        let ctbound = itv1 |> itypevar_apply is |> ctype_of_bound bf in
          match itv1, itv2, ctbound with
            | CTInt (n1, ie), CTInt (n2, IEVar iv), CTInt (n3, ib) when n1 = n2 && n2 = n3 -> bounded_refine_index is ie iv ib
            | CTRef (_, ie), CTRef (_, IEVar iv), CTRef (_, ib)                            -> bounded_refine_index is ie iv ib
            | _                                                                            -> fail_constraint is itc

type cstrmap = itypevarcstr M.IntMap.t

module CSMPrinter = P.MakeMapPrinter(M.IntMap)

let d_cstrmap =
  CSMPrinter.d_map "\n" (fun () -> P.num) d_itypevarcstr

let mk_cstrmap (itcs: itypevarcstr list): cstrmap =
  List.fold_left (fun cm itc -> M.IntMap.add itc.itcid itc cm) M.IntMap.empty itcs

module IM = M.MapWithDefault(struct
                               type t = int
                               let compare = compare

                               type v = int list
                               let default = []
                             end)

type cstrdepmap = int list IM.t (* indexvar -> indexcstr deps *)

module CDMPrinter = P.MakeMapPrinter(IM)

let d_cstrdepmap =
  CDMPrinter.d_map "\n" d_indexvar (P.d_list ", " (fun () -> P.num))

let add_cstrdep (cdm: cstrdepmap) (itc: itypevarcstr): cstrdepmap =
  match itc.itcdesc with
    | ISubtype (itv, _) | IDSubtype (itv, _, _, _) ->
        itv |> itypevar_indexvars |> List.fold_left (fun cdm iv -> IM.add iv (itc.itcid :: IM.find iv cdm) cdm) cdm

let mk_cstrdepmap (itcs: itypevarcstr list): cstrdepmap =
  List.fold_left add_cstrdep IM.empty itcs

(* Probably want to introduce some kind of rank on constraints, but if
   we traverse the program in the right order, id is a reasonable
   approximation. *)
module WkList = Heaps.Functional(struct
                                   type t = int
                                   let compare = compare
                                 end)

let wklist_push (wkl: WkList.t) (itcids: int list): WkList.t =
  List.fold_left (M.flip WkList.add) wkl itcids

let rec iter_solve (cm: cstrmap) (cdm: cstrdepmap) (wkl: WkList.t) (is: indexsol): indexsol =
  match try Some (WkList.maximum wkl) with Heaps.EmptyHeap -> None with
    | None       -> is
    | Some itcid ->
        let itc       = M.IntMap.find itcid cm in
        let wkl       = WkList.remove wkl in
        let succs     = itypevarcstr_rhs_var itc |> function Some iv -> IM.find iv cdm | None -> [] in
        let (wkl, is) = if itypevarcstr_sat is itc then (wkl, is) else (wklist_push wkl succs, refine_itypevarcstr is itc) in
          iter_solve cm cdm wkl is

let solve (itcs: itypevarcstr list): indexsol =
  let cm  = mk_cstrmap itcs in
  let _   = if Cs.ck_olev Cs.ol_solve then P.printf "Constraints:\n\n%a\n\n" d_cstrmap cm |> ignore in
  let cdm = mk_cstrdepmap itcs in
  let _   = if Cs.ck_olev Cs.ol_solve then P.printf "Constraint dependencies:\n\n%a\n\n" d_cstrdepmap cdm |> ignore in
  let wkl = itcs |> List.map itypevarcstr_id |> wklist_push WkList.empty in
    iter_solve cm cdm wkl IndexSol.empty

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

type varenv = itypevar VM.t

type funenv = ifunvar VM.t

type builtinenv = ifunvar VM.t

type env = varenv * funenv

let rec constrain_exp (env: env): C.exp -> itypevar * itypevarcstr list = function
  | C.Const c                     -> let itv = c |> ctype_of_const |> itypevar_of_ctype in (itv, [])
  | C.Lval lv | C.StartOf lv      -> constrain_lval env lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr c
  | C.CastE (ct, e)               -> constrain_cast env ct e
  | C.SizeOf t                    -> constrain_sizeof t
  | e                             -> E.s <| C.error "Unimplemented constrain_exp_aux: %a@!@!" C.d_exp e

and constrain_lval ((ve, _) as env: env): C.lval -> itypevar * itypevarcstr list = function
  | (C.Var v, C.NoOffset)       -> begin try (VM.find v ve, []) with Not_found -> halt <| C.error "Variable not found: %s\n\n" v.C.vname end
  | (C.Mem e, C.NoOffset) as lv ->
      let itv, cs = constrain_exp env e in
        begin match itv with
          | CTRef (s, ie) -> (lv |> C.typeOfLval |> SI.fresh_heaptype |> itypevar_of_ctype, cs)
          | _             -> E.s <| C.bug "fresh_ctvref gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_unop (op: C.unop) (env: env) (t: C.typ) (e: C.exp): itypevar * itypevarcstr list =
  let itv, cs = constrain_exp env e in
    (apply_unop t op, cs)

and apply_unop (rt: C.typ): C.unop -> itypevar = function
  | C.LNot -> CTInt (CM.typ_width rt, IEConst Index.nonneg)
  | C.BNot -> CTInt (CM.typ_width rt, IEConst Index.top)
  | C.Neg  -> CTInt (CM.typ_width rt, IEConst Index.top)

and constrain_binop (op: C.binop) (env: env) (t: C.typ) (e1: C.exp) (e2: C.exp): itypevar * itypevarcstr list =
  let itv1, cs1 = constrain_exp env e1 in
  let itv2, cs2 = constrain_exp env e2 in
  let itv, co   = apply_binop op t e2 itv1 itv2 in
    (itv, M.maybe_cons co (cs1 @ cs2))

and apply_binop: C.binop -> C.typ -> C.exp -> itypevar -> itypevar -> itypevar * itypevarcstr option = function
  | C.PlusA                                 -> apply_arithmetic (fun ie1 ie2 -> IEPlus (ie1, 1, ie2))
  | C.MinusA                                -> apply_arithmetic (fun ie1 ie2 -> IEMinus (ie1, 1, ie2))
  | C.Mult                                  -> apply_arithmetic (fun ie1 ie2 -> IEMult (ie1, ie2))
  | C.Div                                   -> apply_arithmetic (fun ie1 ie2 -> IEDiv (ie1, ie2))
  | C.PlusPI | C.IndexPI                    -> apply_ptrarithmetic (fun ie1 x ie2 -> IEPlus (ie1, x, ie2))
  | C.MinusPI                               -> apply_ptrarithmetic (fun ie1 x ie2 -> IEMinus (ie1, x, ie2))
  | C.MinusPP                               -> apply_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> apply_rel
  | C.Mod                                   -> apply_unknown
  | C.BAnd | C.BOr | C.BXor                 -> apply_unknown
  | C.Shiftlt | C.Shiftrt                   -> apply_unknown
  | bop                                     -> E.s <| C.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic (f: indexexp -> indexexp -> indexexp) (rt: C.typ) (_: C.exp) (itv1: itypevar) (itv2: itypevar): itypevar * itypevarcstr option =
  match itv1, itv2 with
    | (CTInt (n1, ie1), CTInt (n2, ie2)) -> (CTInt (CM.typ_width rt, f ie1 ie2), None)
    | _                                  -> E.s <| C.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic (f: indexexp -> int -> indexexp -> indexexp) (pt: C.typ) (eoffset: C.exp) (itv1: itypevar) (itv2: itypevar): itypevar * itypevarcstr option =
  match C.unrollType pt, itv1, itv2 with
    | C.TPtr (t, _), CTRef (s, ie1), CTInt (n, ie2) when n = CM.int_width ->
        begin match eoffset with
          | C.Const _                     -> (CTRef (s, f ie1 (CM.typ_width t) ie2), None)
          | C.Lval (C.Var vi, C.NoOffset) ->
              let iv = IEVar (fresh_indexvar ()) in
                (CTRef (s, f ie1 (CM.typ_width t) iv), Some (mk_idsubtypecstr (CTInt (n, ie2)) (CTInt (n, iv)) vi ("NNEG", bound_nonneg)))
          | _ -> halt <| C.bug "Pointer arithmetic offset isn't variable or const\n"
        end
    | _ -> E.s <| C.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (pt: C.typ) (_: C.exp) (_: itypevar) (_: itypevar): itypevar * itypevarcstr option =
  (CTInt (CM.typ_width !C.upointType, IEConst Index.top), None)

and apply_rel (_: C.typ) (_: C.exp) (_: itypevar) (_: itypevar): itypevar * itypevarcstr option =
  (CTInt (CM.int_width, IEConst Index.nonneg), None)

and apply_unknown (rt: C.typ) (_: C.exp) (_: itypevar) (_: itypevar): itypevar * itypevarcstr option =
  (CTInt (CM.typ_width rt, IEConst Index.top), None)

and constrain_constptr: C.constant -> itypevar * itypevarcstr list = function
  | C.CStr _                                 -> (CTRef (S.none, IEConst (Index.IInt 0)), [])
  | C.CInt64 (v, ik, so) when v = Int64.zero -> (CTRef (S.none, IEConst Index.IBot), [])
  | c                                        -> halt <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast (env: env) (ct: C.typ) (e: C.exp): itypevar * itypevarcstr list =
  let itv, cs = constrain_exp env e in
    match C.unrollType ct, C.unrollType <| C.typeOf e with
      | C.TFloat (fk, _), _        -> (CTInt (CM.bytesSizeOfFloat fk, IEConst Index.top), cs)
      | C.TInt (ik, _), C.TFloat _ -> (CTInt (C.bytesSizeOfInt ik, IEConst Index.top), cs)
      | C.TInt (ik, _), C.TPtr _   -> (CTInt (C.bytesSizeOfInt ik, IEConst Index.nonneg), cs)
      | C.TInt (ik, _), C.TInt _   ->
          begin match itv with
            | CTInt (n, ie) ->
                let iec =
                  if n <= C.bytesSizeOfInt ik then
                    (* pmr: what about the sign bit?  this may not always be safe *)
                    if C.isSigned ik then ie else IEUnsign ie
                  else if not !Constants.safe then begin
                    C.warn "Unsoundly assuming cast is lossless@!@!" |> ignore;
                    if C.isSigned ik then ie else IEUnsign ie
                  end else
                    IEConst Index.top
                in (CTInt (C.bytesSizeOfInt ik, iec), cs)
            | _ -> halt <| C.error "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> (itv, cs)

and constrain_sizeof (t: C.typ): itypevar * itypevarcstr list =
  (CTInt (CM.int_width, IEConst (Index.IInt (CM.typ_width t))), [])

let constrain_return (env: env) (rtv: itypevar): C.exp option -> itypevarcstr list = function
    | None   -> if is_void rtv then [] else halt <| C.error "Returning void value for non-void function\n\n"
    | Some e ->
        let itv, cs = constrain_exp env e in
          mk_isubtypecstr itv rtv :: cs

let constrain_arg (env: env) (e: C.exp) ((itvs, css): itypevar list * itypevarcstr list list): itypevar list * itypevarcstr list list =
  let itv, cs = constrain_exp env e in
    (itv :: itvs, cs :: css)

let constrain_args (env: env) (args: C.exp list): itypevar list * itypevarcstr list list =
  List.fold_right (constrain_arg env) args ([], [])

let constrain_app ((_, fe) as env: env) (f: C.varinfo) (lvo: C.lval option) (args: C.exp list): itypevarcstr list list =
  let itvs, css = constrain_args env args in
  let ftv       = try VM.find f fe with Not_found -> halt <| C.error "Couldn't find function %a (missing prototype or spec?)\n\n" CM.d_var f in
  let itvfs     = List.map snd ftv.args in
  let _         = if not (List.length itvs = List.length itvfs) then 
                  (halt <| C.errorLoc !C.currentLoc "bad-arguments") in
  let css       = (Misc.do_catch "HERE3" (List.map2 (fun itva itvf -> mk_isubtypecstr itva itvf) itvs) itvfs) 
                  :: css in
    match lvo with
      | None    -> css
      | Some lv ->
          let itvlv, cs2 = constrain_lval env lv in
            (mk_isubtypecstr ftv.ret itvlv :: cs2) :: css

let constrain_instr_aux (env: env) (css: itypevarcstr list list) (i: C.instr): itypevarcstr list list =
  let _ = C.currentLoc := C.get_instrLoc i in
    match i with
      | C.Set (lv, e, _) ->
          let itv1, cs1 = constrain_lval env lv in
          let itv2, cs2 = constrain_exp env e in
            (mk_isubtypecstr itv2 itv1 :: cs1) :: cs2 :: css
      | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
          if not !Constants.safe 
          then C.warn "Unsoundly ignoring vararg call to %a@!@!" CM.d_var f |> ignore 
          else E.s <| C.error "Can't handle varargs";
          (constrain_args env args |> snd |> List.concat) :: css
      | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, _) ->
          (constrain_app env f lvo args |> List.concat) :: css
      | _ -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr (env: env) (is: C.instr list): itypevarcstr list =
  is |> List.fold_left (constrain_instr_aux env) [] |> List.concat

let constrain_stmt (env: env) (rtv: itypevar) (s: C.stmt): itypevarcstr list =
  let _ = C.currentLoc := C.get_stmtLoc s.C.skind in
    match s.C.skind with
      | C.Instr is             -> constrain_instr env is
      | C.If (e, _, _, loc)    -> constrain_exp env e |> snd  (* we'll visit the subblocks later *)
      | C.Break _              -> []
      | C.Continue _           -> []
      | C.Goto _               -> []
      | C.Block _              -> []                              (* we'll visit this later as we iterate through blocks *)
      | C.Loop (_, _, _, _)    -> []                              (* ditto *)
      | C.Return (rexp, loc)   -> constrain_return env rtv rexp
      | _                      -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let maybe_fresh (v: C.varinfo): (C.varinfo * itypevar) option =
  let _ = C.currentLoc := v.C.vdecl in
  let t = C.unrollType v.C.vtype in
    match t with
      | C.TInt _
      | C.TFloat _
      | C.TPtr _
      | C.TArray _
      | C.TEnum _ -> Some (v, fresh_itypevar t)
      | _          ->
          let _ = if !Constants.safe then C.error "not freshing local %s" v.C.vname |> ignore in
            C.warn "Not freshing local %s of tricky type %a@!@!" v.C.vname C.d_type t |> ignore; None

let fresh_vars (vs: C.varinfo list): (C.varinfo * itypevar) list =
  Misc.map_partial maybe_fresh vs

let constrain_phi_defs (ve: varenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): itypevarcstr list =
  let _ = C.currentLoc := vphi.C.vdecl in
    List.map (fun (_, vdef) -> mk_isubtypecstr (VM.find vdef ve) (VM.find vphi ve)) vdefs

let constrain_phis (ve: varenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): itypevarcstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let dump_constraints (fn: string) (ftv: ifunvar) (cs: itypevarcstr list): unit =
  let _ = P.printf "Constraints for %s:\n\n" fn in
  let _ = P.printf "%a\n" d_ifunvar ftv in
  let _ = P.printf "%a\n\n" (P.d_list "\n" d_itypevarcstr) cs in
    ()

let constrain_fun (fe: funenv) (ve: varenv) (ftv: ifunvar) ({ST.fdec = fd; ST.phis = phis; ST.cfg = cfg}: ST.ssaCfgInfo): varenv * itypevarcstr list =
  let bodyformals = fresh_vars fd.C.sformals in
  let locals      = fresh_vars fd.C.slocals in
  let vars        = locals @ bodyformals in
  let ve          = List.fold_left (fun ve (v, itv) -> VM.add v itv ve) ve vars in
  let _           = C.currentLoc := fd.C.svar.C.vdecl in
  let formalcs    = Misc.do_catch "HERE4" (List.map2 (fun (_, at) (_, itv) ->
    mk_isubtypecstr at itv) ftv.args) bodyformals in
  let phics       = constrain_phis ve phis in
  let env         = (ve, fe) in
  let css         = Array.fold_left (fun css b -> constrain_stmt env ftv.ret b.Ssa.bstmt :: css) [] cfg.Ssa.blocks in
  let cs          = formalcs :: phics :: css |> List.concat in
  let _           = if Cs.ck_olev Cs.ol_solve then dump_constraints fd.C.svar.C.vname ftv cs in
    (ve, cs)

let fresh_fun_typ (fd: C.fundec): ifunvar =
  let rty, ftyso, _, _ = C.splitFunctionType fd.C.svar.C.vtype in
  let fctys            = match ftyso with None -> [] | Some ftys -> List.map (fun (fn, fty, _) -> (fn, fresh_itypevar fty)) ftys in
    mk_cfun [] fctys (fresh_itypevar rty) SLM.empty SLM.empty

type indextyping = (cfun * ctype VM.t) VM.t

let d_indextyping () (it: indextyping): P.doc =
     it
  |> M.flip (VM.fold (fun f t it -> if CM.definedHere f then VM.add f t it else it)) VM.empty
  |> CM.VarMapPrinter.d_map
      ~dmaplet:(fun d1 d2 -> P.dprintf "%t\n%t" (fun () -> d1) (fun () -> d2))
      "\n\n"
      CM.d_var
      (fun () (cf, vm) -> P.dprintf "%a\n\nLocals:\n%a\n\n" d_cfun cf (CM.VarMapPrinter.d_map "\n" CM.d_var d_ctype) vm) ()

type dcheck = C.varinfo * FI.refctype

let d_dcheck () ((vi, rt): dcheck): P.doc =
  P.dprintf "%s :: %a" vi.C.vname FI.d_refctype rt

let get_cstr_dcheck (is: indexsol): itypevarcstr -> dcheck option = function
  | {itcdesc = IDSubtype (itv1, _, v, ((_, fb) as bf))} ->
      let ct = itypevar_apply is itv1 in
        if is_subitypevar is itv1 (ct |> ctype_of_bound bf |> itypevar_of_ctype) then
          None
        else
          Some (v, ct |> fb |> snd)
  | _ -> None

(* API *)
let infer_fun_indices (ctenv: cfun VM.t) (ve: ctype VM.t) (scim: ST.ssaCfgInfo VM.t) (cf: cfun) (sci: ST.ssaCfgInfo): ctype VM.t * dcheck list =
  let fe, ve = VM.map ifunvar_of_cfun ctenv, VM.map itypevar_of_ctype ve in
  let ve, cs = constrain_fun fe ve (ifunvar_of_cfun cf) sci in
  let is     = solve cs in
    (VM.map (itypevar_apply is) ve, M.map_partial (get_cstr_dcheck is) cs)
