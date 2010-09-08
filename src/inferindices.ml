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

module IndexExp = struct
  type t =
    | Const  of Index.t
    | Var    of indexvar
    | Plus   of t * (* RHS scale: *) int * t
    | Minus  of t * (* RHS scale: *) int * t
    | Mult   of t * t
    | Div    of t * t
    | Unsign of t

  let rec d_indexexp () = function
    | Const i                 -> Index.d_index () i
    | Var iv                  -> d_indexvar () iv
    | Plus (ie1, scale, ie2)  -> P.dprintf "%a + %d * %a" d_indexexp ie1 scale d_indexexp ie2
    | Minus (ie1, scale, ie2) -> P.dprintf "%a - %d * %a" d_indexexp ie1 scale d_indexexp ie2
    | Mult (ie1, ie2)         -> P.dprintf "%a * %a" d_indexexp ie1 d_indexexp ie2
    | Div (ie1, ie2)          -> P.dprintf "%a / %a" d_indexexp ie1 d_indexexp ie2
    | Unsign ie               -> P.dprintf "(unsigned) %a" d_indexexp ie

  let vars_aux = function
    | Plus (ie1, _, ie2)
    | Minus (ie1, _, ie2)
    | Mult (ie1, ie2)
    | Div (ie1, ie2) -> ([ie1; ie2], [])
    | Const _        -> ([], [])
    | Var iv         -> ([], [iv])
    | Unsign ie      -> ([ie], [])

  let vars ie =
    M.expand vars_aux [ie] []

  let rec apply is = function
    | Const i             -> i
    | Var iv              -> IndexSol.find iv is
    | Unsign ie           -> Index.unsign (apply is ie)
    | Plus (ie1, x, ie2)  -> Index.plus (apply is ie1) (Index.scale x <| apply is ie2)
    | Minus (ie1, x, ie2) -> Index.minus (apply is ie1) (Index.scale x <| apply is ie2)
    | Mult (ie1, ie2)     -> Index.mult (apply is ie1) (apply is ie2)
    | Div (ie1, ie2)      -> Index.div (apply is ie1) (apply is ie2)
end

module IE = IndexExp

let refine_index (is: indexsol) (ie: IE.t) (iv: indexvar): indexsol =
  IndexSol.add iv (Index.lub (IE.apply is ie) (IndexSol.find iv is)) is

let bounded_refine_index (is: indexsol) (ie: IE.t) (iv: indexvar) (ibound: Index.t): indexsol =
  let is = refine_index is ie iv in
    if Index.is_subindex (IndexSol.find iv is) ibound then is else IndexSol.add iv ibound is

(******************************************************************************)
(****************************** Type Constraints ******************************)
(******************************************************************************)

module IndexExpRefinement = struct
  type t = IE.t

  let d_refinement = IE.d_indexexp

  let lub _ _       = assert false
  let is_subref _ _ = assert false
  let of_const _    = assert false
end

module ITV = Ctypes.Make (IndexExpRefinement)

let itypevar_top = Top (IE.Const Index.top)

let itypevar_indexvars = function
  | Int (_, ie) | Ref (_, ie) | Top (ie) -> IE.vars ie

let itypevar_of_ctype = function
  | Int (n, i) -> Int (n, IE.Const i)
  | Ref (s, i) -> Ref (s, IE.Const i)
  | Top (i)    -> Top (IE.Const i)


let ifunvar_of_cfun cf =
  I.CFun.map itypevar_of_ctype cf

let fresh_itypevar t =
  match C.unrollType t with
    | C.TInt (ik, _)        -> Int (C.bytesSizeOfInt ik, IE.Var (fresh_indexvar ()))
    | C.TEnum (ei, _)       -> Int (C.bytesSizeOfInt ei.C.ekind, IE.Var (fresh_indexvar ()))
    | C.TFloat _            -> Int (CM.typ_width t, IE.Const Index.top)
    | C.TVoid _             -> itypevar_of_ctype void_ctype
    | C.TPtr (C.TFun _ , _) -> Top (IE.Const Index.top)
    | C.TPtr _ | C.TArray _ -> Ref (S.none, IE.Var (fresh_indexvar ()))
    | t                     -> E.s <| C.bug "Unimplemented fresh_itypevar: %a@!@!" C.d_type t

let itypevar_apply is itv =
  ITV.CType.map (IE.apply is) itv

let is_subitypevar is itv1 itv2 =
  match itv1, itv2 with
    | Int (n1, ie1), Int (n2, ie2) when n1 = n2 -> Index.is_subindex (IE.apply is ie1) (IE.apply is ie2)
    | Ref (_, ie1), Ref (_, ie2)                -> Index.is_subindex (IE.apply is ie1) (IE.apply is ie2)
    | _                                         -> false

type boundfun = string * (ctype -> ctype * FI.refctype)

let ctype_of_bound ((_, fbound): boundfun) (ctv: ctype): ctype =
  ctv |> fbound |> fst

let bound_nonneg (ct: ctype): ctype * FI.refctype =
  match ct with
    | Int (w, Index.ISeq (n, p, PosNeg)) ->
        let vv   = Ast.Symbol.value_variable Ast.Sort.t_int in
        let pred = Ast.pAtom (Ast.eVar vv, Ast.Ge, Ast.eCon (Ast.Constant.Int 0)) in
        (Int (w, Index.ISeq (n, p, Pos)), FI.t_pred ct vv pred)
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
  d_pretypecstrdesc I.CType.d_ctype () ctcd

type itypevarcstrdesc = ITV.CType.t pretypecstrdesc

let d_itypevarcstrdesc () (itcd: itypevarcstrdesc): P.doc =
  d_pretypecstrdesc ITV.CType.d_ctype () itcd

let itypevarcstrdesc_apply (is: indexsol) (itcd: itypevarcstrdesc): ctypecstrdesc =
  pretypecstrdesc_apply is itypevar_apply itcd

type 'a pretypecstr = {itcid: int; itcdesc: 'a pretypecstrdesc; itcloc: C.location}

let d_pretypecstr (d_type: unit -> 'a -> P.doc) () ({itcid = id; itcdesc = pitcd; itcloc = loc}: 'a pretypecstr): P.doc =
  P.dprintf "%a: %a" C.d_loc loc (d_pretypecstrdesc d_type) pitcd

let pretypecstr_apply (is: indexsol) (app_type: indexsol -> 'a -> 'b) (pitc: 'a pretypecstr): 'b pretypecstr =
  {pitc with itcdesc = pretypecstrdesc_apply is app_type pitc.itcdesc}

type ctypecstr = ctype pretypecstr

let d_ctypecstr () (ctc: ctypecstr): P.doc =
  d_pretypecstr I.CType.d_ctype () ctc

type itypevarcstr = ITV.CType.t pretypecstr

let d_itypevarcstr () (itc: itypevarcstr): P.doc =
  d_pretypecstr ITV.CType.d_ctype () itc

let itypevarcstr_apply (is: indexsol) (itc: itypevarcstr): ctypecstr =
  pretypecstr_apply is itypevar_apply itc

let (fresh_itypevarcstrid, reset_fresh_itypevarcstrids) = M.mk_int_factory ()

let mk_isubtypecstr itv1 itv2 =
  {itcid = fresh_itypevarcstrid (); itcloc = !C.currentLoc; itcdesc = ISubtype (itv1, itv2)}

let mk_idsubtypecstr itv1 itv2 v bf =
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
          | Int (n1, ie), Int (n2, IE.Var iv) when n1 = n2 -> refine_index is ie iv
          | Ref (_, ie), Ref (_, IE.Var iv)                -> refine_index is ie iv
          | _                                              -> fail_constraint is itc
        end
    | IDSubtype (itv1, itv2, _, bf) ->
        let ctbound = itv1 |> itypevar_apply is |> ctype_of_bound bf in
          match itv1, itv2, ctbound with
            | Int (n1, ie), Int (n2, IE.Var iv), Int (n3, ib) when n1 = n2 && n2 = n3 -> bounded_refine_index is ie iv ib
            | Ref (_, ie), Ref (_, IE.Var iv), Ref (_, ib)                            -> bounded_refine_index is ie iv ib
            | _                                                                       -> fail_constraint is itc

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

type varenv = ITV.CType.t VM.t

type funenv = ITV.CFun.t VM.t

type builtinenv = ITV.CFun.t VM.t

type env = varenv * funenv

let rec constrain_exp env e = match e with 
  | C.Const c                     -> let itv = c |> I.CType.of_const |> itypevar_of_ctype in (itv, [])
  | C.Lval lv | C.StartOf lv      -> constrain_lval env lv
  | C.UnOp (uop, e, t)            -> constrain_unop uop env t e
  | C.BinOp (bop, e1, e2, t)      -> constrain_binop bop env t e1 e2
  | C.CastE (C.TPtr _, C.Const c) -> constrain_constptr c
  | C.CastE (ct, e)               -> constrain_cast env ct e
  | C.SizeOf t                    -> constrain_sizeof t
  | C.AddrOf lv                   -> constrain_addrof env lv
  | e                             -> E.s <| C.error "Unimplemented constrain_exp: %a@!@!" C.d_exp e

and constrain_addrof env = function
  | (C.Var v, _) as lv when CM.is_fun v ->
      let _ = CM.g_error !Cs.safe "constrain_exp cannot handle addrOf: %a@!@!" C.d_lval lv |> CM.g_halt !Cs.safe in
      (itypevar_top, [])

and constrain_lval ((ve, _) as env) = function
  | (C.Var v, C.NoOffset)       -> begin try (VM.find v ve, []) with Not_found -> halt <| C.error "Variable not found: %s\n\n" v.C.vname end
  | (C.Mem e, C.NoOffset) as lv ->
      let itv, cs = constrain_exp env e in
        begin match itv with
          | Ref (s, ie) -> (lv |> C.typeOfLval |> SI.fresh_heaptype |> itypevar_of_ctype, cs)
          | _           -> E.s <| C.bug "fresh_ctvref gave back non-ref type in constrain_lval@!@!"
        end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

and constrain_unop op env t e =
  let itv, cs = constrain_exp env e in
  (apply_unop t op, cs)

and apply_unop rt = function
  | C.LNot -> Int (CM.typ_width rt, IE.Const Index.nonneg)
  | C.BNot -> Int (CM.typ_width rt, IE.Const Index.top)
  | C.Neg  -> Int (CM.typ_width rt, IE.Const Index.top)

and constrain_binop op env t e1 e2 =
  let itv1, cs1 = constrain_exp env e1 in
  let itv2, cs2 = constrain_exp env e2 in
  let itv, co   = apply_binop op t e2 itv1 itv2 in
    (itv, M.maybe_cons co (cs1 @ cs2))

and apply_binop = function
  | C.PlusA                                 -> apply_arithmetic (fun ie1 ie2 -> IE.Plus (ie1, 1, ie2))
  | C.MinusA                                -> apply_arithmetic (fun ie1 ie2 -> IE.Minus (ie1, 1, ie2))
  | C.Mult                                  -> apply_arithmetic (fun ie1 ie2 -> IE.Mult (ie1, ie2))
  | C.Div                                   -> apply_arithmetic (fun ie1 ie2 -> IE.Div (ie1, ie2))
  | C.PlusPI | C.IndexPI                    -> apply_ptrarithmetic (fun ie1 x ie2 -> IE.Plus (ie1, x, ie2))
  | C.MinusPI                               -> apply_ptrarithmetic (fun ie1 x ie2 -> IE.Minus (ie1, x, ie2))
  | C.MinusPP                               -> apply_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> apply_rel
  | C.Mod                                   -> apply_unknown
  | C.BAnd | C.BOr | C.BXor                 -> apply_unknown
  | C.Shiftlt | C.Shiftrt                   -> apply_unknown
  | bop                                     -> E.s <| C.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic f rt _ itv1 itv2 =
  match itv1, itv2 with
    | Int (n1, ie1), Int (n2, ie2) -> (Int (CM.typ_width rt, f ie1 ie2), None)
    | _                            -> E.s <| C.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic f pt eoffset itv1 itv2 =
  match C.unrollType pt, itv1, itv2 with
    | C.TPtr (t, _), Ref (s, ie1), Int (n, ie2) when n = CM.int_width ->
        begin match eoffset with
          | C.Const _                     -> (Ref (s, f ie1 (CM.typ_width t) ie2), None)
          | C.Lval (C.Var vi, C.NoOffset) ->
              let iv = IE.Var (fresh_indexvar ()) in
                (Ref (s, f ie1 (CM.typ_width t) iv), Some (mk_idsubtypecstr (Int (n, ie2)) (Int (n, iv)) vi ("NNEG", bound_nonneg)))
          | _ -> halt <| C.bug "Pointer arithmetic offset isn't variable or const\n"
        end
    | _ -> E.s <| C.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus pt _ _ _ =
  (Int (CM.typ_width !C.upointType, IE.Const Index.top), None)

and apply_rel _ _ _ _ =
  (Int (CM.int_width, IE.Const Index.nonneg), None)

and apply_unknown rt _ _ _ =
  (Int (CM.typ_width rt, IE.Const Index.top), None)

and constrain_constptr = function
  | C.CStr _                                 -> (Ref (S.none, IE.Const (Index.IInt 0)), [])
  | C.CInt64 (v, ik, so) when v = Int64.zero -> (Ref (S.none, IE.Const Index.IBot), [])
  | c                                        -> halt <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

and constrain_cast env ct e =
  let itv, cs = constrain_exp env e in
    match C.unrollType ct, C.unrollType <| C.typeOf e with
      | C.TFloat (fk, _), _        -> (Int (CM.bytesSizeOfFloat fk, IE.Const Index.top), cs)
      | C.TInt (ik, _), C.TFloat _ -> (Int (C.bytesSizeOfInt ik, IE.Const Index.top), cs)
      | C.TInt (ik, _), C.TPtr _   -> (Int (C.bytesSizeOfInt ik, IE.Const Index.nonneg), cs)
      | C.TInt (ik, _), C.TInt _   ->
          begin match itv with
            | Int (n, ie) ->
                let iec =
                  if n <= C.bytesSizeOfInt ik then
                    (* pmr: what about the sign bit?  this may not always be safe *)
                    if C.isSigned ik then ie else IE.Unsign ie
                  else if not !Constants.safe then begin
                    C.warn "Unsoundly assuming cast is lossless@!@!" |> ignore;
                    if C.isSigned ik then ie else IE.Unsign ie
                  end else
                    IE.Const Index.top
                in (Int (C.bytesSizeOfInt ik, iec), cs)
            | _ -> halt <| C.error "Got bogus type in contraining int-int cast@!@!"
          end
      | _ -> (itv, cs)

and constrain_sizeof t =
  (Int (CM.int_width, IE.Const (Index.IInt (CM.typ_width t))), [])

let constrain_return env rtv = function
    | None   -> if ITV.CType.is_void rtv then [] else halt <| C.error "Returning void value for non-void function\n\n"
    | Some e ->
        let itv, cs = constrain_exp env e in
          mk_isubtypecstr itv rtv :: cs

let constrain_arg env e (itvs, css) =
  let itv, cs = constrain_exp env e in
  (itv :: itvs, cs :: css)

let constrain_args env args =
  List.fold_right (constrain_arg env) args ([], [])

let constrain_app ((_, fe) as env) f lvo args =
  let itvs, css = constrain_args env args in
  let ftv       = try VM.find f fe with Not_found -> halt <| C.error "Couldn't find function %a (missing prototype or spec?)\n\n" CM.d_var f in
  let itvfs     = List.map snd ftv.args in
  let _         = if not (List.length itvs = List.length itvfs) then 
                  (halt <| C.errorLoc !C.currentLoc "bad-arguments") in
  let css       = (Misc.do_catch "HERE3" (List.map2 (fun itva itvf -> mk_isubtypecstr itva itvf) itvs) itvfs) 
                  :: css in
  match lvo with
  | None    -> css
  | Some lv -> let itvlv, cs2 = constrain_lval env lv in
              (mk_isubtypecstr ftv.ret itvlv :: cs2) :: css

let constrain_instr_aux env i =
  let loc = i |> C.get_instrLoc >> (:=) C.currentLoc in
  match i with
  | C.Set (lv, e, _) ->
      let itv1, cs1 = constrain_lval env lv in
      let itv2, cs2 = constrain_exp env e in
      (mk_isubtypecstr itv2 itv1 :: cs1) @ cs2
  | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
      let _ = CM.g_errorLoc !Cs.safe loc "constrain_instr_aux: vararg-call %a@!@!" C.dn_instr i |> CM.g_halt !Cs.safe in
      (constrain_args env args |> snd |> List.concat)
  | C.Call (lvo, C.Lval (C.Var f, C.NoOffset), args, _) ->
      (constrain_app env f lvo args |> List.concat)
  | C.Call (_, C.Lval (C.Mem _, _), _, _) ->
      let _ = CM.g_errorLoc !Cs.safe loc "constrain_instr_aux: funptr-call %a@!@!" C.dn_instr i |> CM.g_halt !Cs.safe in
      []
  | _ -> E.s <| C.bug "Unimplemented constrain_instr : %a@!@!" C.dn_instr i

let is_dsubtype = function
  | {itcdesc = IDSubtype _} -> true
  | _                       -> false

let constrain_instr env i =
  let cs    = constrain_instr_aux env i in
  let dsubs = List.filter is_dsubtype cs in
    (cs, dsubs)

let constrain_instrs env is =
  let css, dsubss = is |> List.map (constrain_instr env) |> List.split in
    (List.concat css, dsubss)

let constrain_stmt env rtv s =
  match s.C.skind with
    | C.Instr is             -> constrain_instrs env is
    | C.If (e, _, _, loc)    -> (constrain_exp env e |> snd, [])
    | C.Break _              -> ([], [])
    | C.Continue _           -> ([], [])
    | C.Goto _               -> ([], [])
    | C.Block _              -> ([], [])                        (* we'll visit this later as we iterate through blocks *)
    | C.Loop (_, _, _, _)    -> ([], [])                        (* ditto *)
    | C.Return (rexp, loc)   -> (constrain_return env rtv rexp, [])
    | _                      -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_block i env bdsubs rtv s =
  let _          = C.currentLoc := C.get_stmtLoc s.C.skind in
  let cs, dsubss = constrain_stmt env rtv s in
  let _          = bdsubs.(i) <- dsubss in
    cs

let maybe_fresh v =
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

let fresh_vars vs =
  Misc.map_partial maybe_fresh vs

let constrain_phi_defs (ve: varenv) ((vphi, vdefs): C.varinfo * (int * C.varinfo) list): itypevarcstr list =
  let _ = C.currentLoc := vphi.C.vdecl in
    List.map (fun (_, vdef) -> mk_isubtypecstr (VM.find vdef ve) (VM.find vphi ve)) vdefs

let constrain_phis (ve: varenv) (phis: (C.varinfo * (int * C.varinfo) list) list array): itypevarcstr list =
  Array.to_list phis |> List.flatten |> List.map (constrain_phi_defs ve) |> List.concat

let dump_constraints fn ftv cs =
  let _ = P.printf "Constraints for %s:\n\n" fn in
  let _ = P.printf "%a\n" ITV.CFun.d_cfun ftv in
  let _ = P.printf "%a\n\n" (P.d_list "\n" d_itypevarcstr) cs in
    ()

let constrain_fun fe ve ftv {ST.fdec = fd; ST.phis = phis; ST.cfg = cfg} bdsubs =
  let bodyformals = fresh_vars fd.C.sformals in
  let locals      = fresh_vars fd.C.slocals in
  let vars        = locals @ bodyformals in
  let ve          = List.fold_left (fun ve (v, itv) -> VM.add v itv ve) ve vars in
  let _           = C.currentLoc := fd.C.svar.C.vdecl in
  let formalcs    = Misc.do_catch "HERE4" (List.map2 (fun (_, at) (_, itv) ->
    mk_isubtypecstr at itv) ftv.args) bodyformals in
  let phics       = constrain_phis ve phis in
  let env         = (ve, fe) in
  let css         = M.array_fold_lefti (fun i css b -> constrain_block i env bdsubs ftv.ret b.Ssa.bstmt :: css) [] cfg.Ssa.blocks in
  let cs          = formalcs :: phics :: css |> List.concat in
  let _           = if Cs.ck_olev Cs.ol_solve then dump_constraints fd.C.svar.C.vname ftv cs in
    (ve, cs)

let fresh_fun_typ fd =
  let rty, ftyso, _, _ = C.splitFunctionType fd.C.svar.C.vtype in
  let fctys            = match ftyso with None -> [] | Some ftys -> List.map (fun (fn, fty, _) -> (fn, fresh_itypevar fty)) ftys in
    ITV.CFun.make [] fctys (fresh_itypevar rty) SLM.empty SLM.empty

type indextyping = (cfun * ctype VM.t) VM.t

let d_indextyping () (it: indextyping): P.doc =
     it
  |> M.flip (VM.fold (fun f t it -> if CM.definedHere f then VM.add f t it else it)) VM.empty
  |> CM.VarMapPrinter.d_map
      ~dmaplet:(fun d1 d2 -> P.dprintf "%t\n%t" (fun () -> d1) (fun () -> d2))
      "\n\n"
      CM.d_var
      (fun () (cf, vm) -> P.dprintf "%a\n\nLocals:\n%a\n\n" I.CFun.d_cfun cf (CM.VarMapPrinter.d_map "\n" CM.d_var I.CType.d_ctype) vm) ()

type dcheck        = C.varinfo * FI.refctype
type block_dchecks = dcheck list list

let d_dcheck () ((vi, rt): dcheck): P.doc =
  P.dprintf "%s :: %a" vi.C.vname FI.d_refctype rt

let d_instr_dchecks () dcks =
  P.dprintf "  %a" (P.d_list ", " d_dcheck) dcks

let d_blocks_dchecks () bdcks =
  P.docArray ~sep:(P.text "\n") begin fun i dckss ->
    P.dprintf "Block %d:\n%a\n" i (P.d_list "\n" d_instr_dchecks) dckss
  end () bdcks

let get_blocks_dchecks is bdsubs =
  let bdcks = Array.create (Array.length bdsubs) [] in
    Array.iteri begin fun i dsubss ->
      bdcks.(i) <- begin
        List.map begin fun dsubs ->
          M.map_partial begin function
            | {itcdesc = IDSubtype (itv1, _, v, ((_, fb) as bf))} ->
                let ct = itypevar_apply is itv1 in
                  if is_subitypevar is itv1 (ct |> ctype_of_bound bf |> itypevar_of_ctype) then
                    None
                  else
                    Some (v, ct |> fb |> snd)
            | _ -> None
          end dsubs
        end
      end dsubss
    end bdsubs;
    bdcks

(* API *)
let infer_fun_indices ctenv ve scim cf sci =
  let fe, ve = VM.map ifunvar_of_cfun ctenv, VM.map itypevar_of_ctype ve in
  let bdsubs = Array.create (Array.length sci.ST.cfg.Ssa.blocks) [] in
  let ve, cs = constrain_fun fe ve (ifunvar_of_cfun cf) sci bdsubs in
  let is     = solve cs in
    (VM.map (itypevar_apply is) ve, get_blocks_dchecks is bdsubs)
