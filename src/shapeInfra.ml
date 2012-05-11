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

open Ctypes
module Misc = FixMisc open Misc.Ops

module C  = Cil
module E  = Errormsg
module CM = CilMisc
module VM = CM.VarMap
module Cs = Constants
module S  = Sloc
module N  = Index
module Ct = I.CType
module FI = FixInterface
module FA = FixAstInterface
module FC = FixConstraint
module A  = Ast
module Sy = A.Symbol
module RS = RefCTypes.Store
module RT = RefCTypes.CType
module RF = RefCTypes.CFun
module Ts = Typespec

let rec typealias_attrs: C.typ -> C.attributes = function
  | C.TNamed (ti, a) -> a @ typealias_attrs ti.C.ttype
  | _                -> []
    
    
let fref_of_ctype loc f = 
  f |> Ts.preRefcfunOfType loc 
    |> Ts.refcfunOfPreRefcfun S.Subst.empty RS.empty
    |> (fun (f,_,_) -> FRef (RF.map (RT.map fst) f, N.of_int 0))
      
(* Note that int refinements always have to include the constant index
   0.  This is because we need to account for the fact that malloc
   allocates a block of zeroes - it would be tedious to account for
   this separately at all malloc call sites, so we just always ensure
   whatever int shape is on the heap always includes 0, which is
   sufficient and unproblematic so far. *)
let fresh_heaptype loc (t: C.typ): ctype =
  let ats1 = typealias_attrs t in
    match C.unrollType t with
      | C.TInt (ik, _)           -> Int (C.bytesSizeOfInt ik, N.top)
      | C.TEnum (ei, _)          -> Int (C.bytesSizeOfInt ei.C.ekind, N.top)
      | C.TFloat _               -> Int (CM.typ_width t, N.top)
      | C.TVoid _                -> void_ctype
      | C.TPtr (C.TFun _ as f,_) -> fref_of_ctype loc f
      (* Will need this once we do inference *)
      (* | C.TPtr ((C.TVoid _) as tb, ats) -> Ctypes.TVar (Typespec.tvarOfAttrs ats) *)
      | C.TPtr (tb, ats) when C.hasAttribute CM.typeVarAttribute ats ->
        Ctypes.TVar (Ctypes.fresh_tvar ()) 
      | C.TPtr (tb, ats) | C.TArray (tb, _, ats) as t ->
          let sl = S.fresh_abstract (CM.srcinfo_of_type t (Some loc)) in
          Typespec.ptrReftypeOfSlocAttrs sl tb ats
          |> Ctypes.ctype_of_refctype
      | _ -> halt <| C.bug "Unimplemented fresh_heaptype: %a@!@!" C.d_type t

let rec base_ctype_of_binop_result = function
  | C.PlusA | C.MinusA | C.Mult | C.Div                     -> base_ctype_of_arith_result
  | C.PlusPI | C.IndexPI | C.MinusPI                        -> base_ctype_of_ptrarith_result
  | C.MinusPP                                               -> base_ctype_of_ptrminus_result
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne                 -> base_ctype_of_rel_result
  | C.LAnd | C.LOr                                          -> base_ctype_of_logical_result
  | C.Mod | C.BAnd | C.BOr | C.BXor | C.Shiftlt | C.Shiftrt -> base_ctype_of_bitop_result
  | bop -> E.s <| C.bug "Unimplemented base_ctype_of_binop_result: %a@!@!" C.d_binop bop

and base_ctype_of_arith_result rt _ _ =
  Int (CM.typ_width rt, Index.top)

and base_ctype_of_ptrarith_result pt ctv1 ctv2 = match C.unrollType pt, ctv1, ctv2 with
  | C.TPtr _, Ref (s, _), Int (n, _) when n = CM.int_width -> Ref (s, Index.top)
  | _ -> E.s <| C.bug "Type mismatch in base_ctype_of_ptrarith_result@!@!"

and base_ctype_of_ptrminus_result _ _ _ =
  Int (CM.typ_width !C.upointType, Index.top)

and base_ctype_of_rel_result _ _ _ =
  Int (CM.int_width, Index.nonneg)

and base_ctype_of_logical_result _ _ _ =
  Int (CM.int_width, Index.nonneg)

and base_ctype_of_bitop_result rt _ _ =
  Int (CM.typ_width rt, Index.top)

and base_ctype_of_unop_result rt = function
  | C.BNot | C.Neg -> Int (CM.typ_width rt, Index.top)
  | C.LNot         -> Int (CM.typ_width rt, Index.nonneg)

let reft_of_ctype ct =
  let vv, p = ScalarCtypes.non_null_pred_of_ctype ct in
    FC.make_reft vv (FI.sort_of_prectype ct) [FC.Conc p]

let fixenv_of_ctypeenv ve =
  VM.fold
    (fun v ct fe -> Sy.SMap.add (Sy.of_string v.C.vname) (reft_of_ctype ct) fe)
    ve Sy.SMap.empty

class exprTyper (ve, fe) = object (self)
  val tbl = Hashtbl.create 17
  val fe  = ref fe
  val fce = fixenv_of_ctypeenv ve

  method ctype_of_exp loc e =
    Misc.do_memo tbl self#ctype_of_exp_aux (loc, e) e

  (* pmr: This is a major hack: the index solver solves for indices,
   * which it turns into predicates, which we're here turning back
   * into predicates.
   *
   * Instead, index inference should return a map from expressions to
   * indices, much like the real solver returns a map from expressions
   * to refined types. We can then avoid this redundancy. *)
  method private ctype_of_exp_aux (loc, e) =
    let ct = self#base_ctype_of_exp loc e in
      match e with
        | C.Lval _ -> ct
        | _        ->
          let so   = FI.sort_of_prectype ct in
          let vv   = Sy.value_variable so in
          let p    = e |> CilInterface.reft_of_cilexp vv |> snd in
          let reft = FC.make_reft vv so [FC.Conc p] in
          let idx  = N.glb (Ct.refinement ct) (N.index_of_reft fce (fun _ -> assert false) reft) in
            Ct.set_refinement ct idx

  method private base_ctype_of_exp loc = function
    | C.Const c                     -> Ct.of_const c
    | C.Lval lv | C.StartOf lv      -> self#base_ctype_of_raw_lval loc lv
    | C.UnOp (uop, e, t)            -> base_ctype_of_unop_result t uop
    | C.BinOp (bop, e1, e2, t)      -> base_ctype_of_binop_result bop t (self#ctype_of_exp loc e1) (self#ctype_of_exp loc e2)
    | C.CastE (C.TPtr (C.TFun _ as f,_), C.Const c) -> self#base_ctype_of_constfptr loc f c
    | C.CastE (C.TPtr _, C.Const c) -> self#base_ctype_of_constptr loc c
    | C.CastE (ct, e)               -> self#base_ctype_of_cast loc ct e
    | C.SizeOf t                    -> Int (CM.int_width, Index.IInt (CM.typ_width t))
    | C.AddrOf lv                   -> self#base_ctype_of_addrof lv
    | e                             -> E.s <| C.error "Unimplemented base_ctype_of_exp: %a@!@!" C.d_exp e

  method private base_ctype_of_constfptr loc f c = match c with
    | C.CInt64 (v, ik, so)
        when v = Int64.zero -> 
      fref_of_ctype loc f |> Misc.flip Ct.set_refinement Index.IBot
      (* let CTypes.FRef (f, _) =  *)
      (*   let fspec = Typespec.preRefcfunOfType loc f in *)
      (*     Ctypes.FRef (Ctypes.RefCTypes.CFun.map *)
      (*                    (Ctypes.RefCTypes.CType.map fst) fspec, *)
      (*                  Index.IBot) *)
            
  method private base_ctype_of_constptr loc c = match c with
    | C.CStr _ ->
        let s = S.fresh_abstract (CM.srcinfo_of_constant c (Some loc)) in 
        Ref (s, Index.IInt 0)
    | C.CInt64 (v, ik, so) 
      when v = Int64.zero ->
        let s = S.fresh_abstract (CM.srcinfo_of_constant c (Some loc)) in 
        Ref (s, Index.IBot)
    | _ -> 
        E.s <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

  method private base_ctype_of_raw_lval loc = function
    | C.Var v, C.NoOffset         -> asserti (VM.mem v ve) "Cannot_find: %s" v.C.vname; VM.find v ve
    | (C.Mem e, C.NoOffset) as lv -> lv |> C.typeOfLval |> fresh_heaptype loc
    | lv                          -> E.s <| C.bug "base_ctype_of_lval got lval with offset: %a@!@!" C.d_lval lv

  method ctype_of_lval loc lv =
    self#ctype_of_exp loc (C.Lval lv)

  method private base_ctype_of_addrof = function
    | C.Var v, C.NoOffset when CM.is_fun v ->
      let fspec,_ = VM.find v !fe in
      FRef (fspec, Index.IInt 0)
    | lv -> 
        E.s <| C.error "Unimplemented base_ctype_of_addrof: %a@!@!" C.d_lval lv

  method private base_ctype_of_cast loc ct e =
    let ctv = self#ctype_of_exp loc e in
      match C.unrollType ct, C.unrollType <| C.typeOf e with
        | C.TInt (ik, _), C.TPtr _   -> Int (C.bytesSizeOfInt ik, Index.nonneg)
        | C.TInt (ik, _), C.TFloat _ -> Int (C.bytesSizeOfInt ik, Index.top)
        | C.TFloat (fk, _), _        -> Int (CM.bytesSizeOfFloat fk, Index.top)
        | C.TInt (ik, _), C.TInt _   ->
          begin match ctv with
            | Int (n, ie) ->
              let iec =
                if n <= C.bytesSizeOfInt ik then
                (* pmr: what about the sign bit?  this may not always be safe *)
                  if C.isSigned ik then ie else Index.unsign ie
                else if not !Cs.safe then begin
                  C.warn "Unsoundly assuming cast is lossless@!@!" |> ignore;
                  if C.isSigned ik then ie else Index.unsign ie
                end else
                  Index.top
              in Int (C.bytesSizeOfInt ik, iec)
            | _ -> E.s <| C.error "Got bogus type in int-int cast@!@!"
          end
        | _ -> ctv 
end
