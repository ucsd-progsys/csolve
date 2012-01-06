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
open Misc.Ops

module C  = Cil
module E  = Errormsg
module CM = CilMisc
module VM = CM.VarMap
module Cs = Constants
module S  = Sloc
module N  = Index
module Ct = I.CType

let rec typealias_attrs: C.typ -> C.attributes = function
  | C.TNamed (ti, a) -> a @ typealias_attrs ti.C.ttype
  | _                -> []

(* Note that int refinements always have to include the constant index
   0.  This is because we need to account for the fact that malloc
   allocates a block of zeroes - it would be tedious to account for
   this separately at all malloc call sites, so we just always ensure
   whatever int shape is on the heap always includes 0, which is
   sufficient and unproblematic so far. *)
let fresh_heaptype (t: C.typ): ctype =
  let ats1 = typealias_attrs t in
    match C.unrollType t with
      | C.TInt (ik, _)                           -> Int (C.bytesSizeOfInt ik, N.top)
      | C.TEnum (ei, _)                          -> Int (C.bytesSizeOfInt ei.C.ekind, N.top)
      | C.TFloat _                               -> Int (CM.typ_width t, N.top)
      | C.TVoid _                                -> void_ctype
      | C.TPtr (tb, ats) | C.TArray (tb, _, ats) as t ->
           Typespec.ptrReftypeOfSlocAttrs (S.fresh_abstract [CM.srcinfo_of_type t None]) tb ats
        |> Ctypes.ctype_of_refctype
      | _ -> halt <| C.bug "Unimplemented fresh_heaptype: %a@!@!" C.d_type t

let rec apply_binop: C.binop -> C.typ -> ctype -> ctype -> ctype = function
  | C.PlusA                                 -> apply_arithmetic Index.plus
  | C.MinusA                                -> apply_arithmetic Index.minus
  | C.Mult                                  -> apply_arithmetic Index.mult
  | C.Div                                   -> apply_arithmetic Index.div
  | C.PlusPI | C.IndexPI                    -> apply_ptrarithmetic (fun i1 x i2 -> Index.plus i1 (Index.scale x i2))
  | C.MinusPI                               -> apply_ptrarithmetic (fun i1 x i2 -> Index.minus i1 (Index.scale x i2))
  | C.MinusPP                               -> apply_ptrminus
  | C.Lt | C.Gt | C.Le | C.Ge | C.Eq | C.Ne -> apply_rel
  | C.LAnd | C.LOr                          -> apply_logical
  | C.Mod                                   -> apply_unknown
  | C.BAnd | C.BOr | C.BXor                 -> apply_unknown
  | C.Shiftlt | C.Shiftrt                   -> apply_unknown
  | bop                                     -> E.s <| C.bug "Unimplemented apply_binop: %a@!@!" C.d_binop bop

and apply_arithmetic f rt ctv1 ctv2 = match ctv1, ctv2 with
  | Int (n1, i1), Int (n2, i2) -> Int (CM.typ_width rt, f i1 i2)
  | _                          -> E.s <| C.bug "Type mismatch in apply_arithmetic@!@!"

and apply_ptrarithmetic f pt ctv1 ctv2 = match C.unrollType pt, ctv1, ctv2 with
  | C.TPtr (t, _), Ref (s, i1), Int (n, i2) when n = CM.int_width -> Ref (s, f i1 (CM.typ_width t) i2)
  | _                                                             -> E.s <| C.bug "Type mismatch in constrain_ptrarithmetic@!@!"

and apply_ptrminus (pt: C.typ) (_: ctype) (_: ctype): ctype =
  Int (CM.typ_width !C.upointType, Index.top)

and apply_rel (_: C.typ) (_: ctype) (_: ctype): ctype =
  Int (CM.int_width, Index.nonneg)

and apply_logical (_: C.typ) (_: ctype) (_: ctype): ctype =
  Int (CM.int_width, Index.nonneg)

and apply_unknown (rt: C.typ) (_: ctype) (_: ctype): ctype =
  Int (CM.typ_width rt, Index.top)

and apply_unop (rt: C.typ): C.unop -> ctype = function
  | C.LNot -> Int (CM.typ_width rt, Index.nonneg)
  | C.BNot -> Int (CM.typ_width rt, Index.top)
  | C.Neg  -> Int (CM.typ_width rt, Index.top)

class exprTyper (ve) = object (self)
  val tbl = Hashtbl.create 17

  method ctype_of_exp e =
    Misc.do_memo tbl self#ctype_of_exp_aux e e

  method private ctype_of_exp_aux = function
    | C.Const c                     -> Ct.of_const c
    | C.Lval lv | C.StartOf lv      -> self#ctype_of_raw_lval lv
    | C.UnOp (uop, e, t)            -> apply_unop t uop
    | C.BinOp (bop, e1, e2, t)      -> apply_binop bop t (self#ctype_of_exp e1) (self#ctype_of_exp e2)
    | C.CastE (C.TPtr _, C.Const c) -> self#ctype_of_constptr c
    | C.CastE (ct, e)               -> self#ctype_of_cast ct e
    | C.SizeOf t                    -> Int (CM.int_width, Index.IInt (CM.typ_width t))
    | C.AddrOf lv                   -> self#ctype_of_addrof lv
    | e                             -> E.s <| C.error "Unimplemented ctype_of_exp_aux: %a@!@!" C.d_exp e

  method private ctype_of_constptr c = match c with  
    | C.CStr _ ->
        let s = S.fresh_abstract [CM.srcinfo_of_constant c None] in 
        Ref (s, Index.IInt 0)
    | C.CInt64 (v, ik, so) 
      when v = Int64.zero ->
        let s = S.fresh_abstract [CM.srcinfo_of_constant c None] in 
        Ref (s, Index.IBot)
    | _ -> 
        E.s <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

  method private ctype_of_raw_lval = function
    | C.Var v, C.NoOffset         -> asserti (VM.mem v ve) "Cannot_find: %s" v.C.vname; VM.find v ve
    | (C.Mem e, C.NoOffset) as lv -> lv |> C.typeOfLval |> fresh_heaptype
    | lv                          -> E.s <| C.bug "ctype_of_lval got lval with offset: %a@!@!" C.d_lval lv

  method ctype_of_lval lv =
    self#ctype_of_exp (C.Lval lv)

  method private ctype_of_addrof = function
    | C.Var v, C.NoOffset when CM.is_fun v ->
      let fspec = Typespec.preRefcfunOfType v.C.vtype in
          FRef (RefCTypes.CFun.map (RefCTypes.CType.map fst) fspec,
                   Index.IInt 0)
        (* Ref (S.fresh_abstract [CM.srcinfo_of_var v None], Index.IInt 0) *)
    | lv -> 
        E.s <| C.error "Unimplemented ctype_of_addrof: %a@!@!" C.d_lval lv

  method private ctype_of_cast ct e =
    let ctv = self#ctype_of_exp e in
      match C.unrollType ct, C.unrollType <| C.typeOf e with
        | C.TInt (ik, _), C.TPtr _     -> Int (C.bytesSizeOfInt ik, Index.nonneg)
        | C.TInt (ik, _), C.TFloat _   -> Int (C.bytesSizeOfInt ik, Index.top)
        | C.TFloat (fk, _), _          -> Int (CM.bytesSizeOfFloat fk, Index.top)
        | C.TInt (ik, _), C.TInt _     ->
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
