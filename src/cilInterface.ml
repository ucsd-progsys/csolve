(*
 * Copyright © 1990-2009 The Regents of the University of California. 
 * All rights reserved. 
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

(****************************************************************)
(******* Interface for converting from Cil to Fixpoint-Ast ******)
(****************************************************************)

module A  = Ast
module E  = A.Expression
module P  = A.Predicate
module Sy = A.Symbol
module CM = CilMisc
module Ct = Ctypes
module FA = FixAstInterface

module Misc = FixMisc open Misc.Ops
open Cil

(****************************************************************)
(************************* Types ********************************)
(****************************************************************)

let index_of_attrs ats = 
  if CM.has_pos_attr ats then Index.nonneg else Index.top

let ctype_of_cilbasetype = function 
  | TVoid ats        -> Ct.Int (0,                       index_of_attrs ats)
  | TInt (ik,   ats) -> Ct.Int (bytesSizeOfInt ik,       index_of_attrs ats)
  | TFloat (fk, ats) -> Ct.Int (CM.bytesSizeOfFloat fk,  index_of_attrs ats)
  | TEnum (ei,  ats) -> Ct.Int (bytesSizeOfInt ei.ekind, index_of_attrs ats)
  | _                -> assertf "ctype_of_cilbasetype: non-base!"

(****************************************************************)
(********************* Constants ********************************)
(****************************************************************)

(* API *)
let expr_of_cilcon = function
  | Cil.CInt64 (i, _, _) -> 
      A.eCon (A.Constant.Int (Int64.to_int i))
  | Cil.CChr c ->
      A.eCon (A.Constant.Int (Char.code c))
  | _ ->
      assertf "TBD: CilInterface.con_of_cilcon unhandled"
(*  | Cil.CStr _        -> Constant.String str
    | Cil.CChr _        -> Constant.Int (int_of_char c) 
    | Cil.CReal (_,_,_) -> Constant.Float f
    | Cil.CWStr _ -> failwith "CIL: Wide character constant not handled yet"
*)

(****************************************************************)
(********************* Expressions/Predicates *******************)
(****************************************************************)

type exp_or_pred = 
  | E of A.expr 
  | P of A.pred

type op = 
  | Bop  of A.bop
  | Bpop of A.bop
  | Brl  of A.brel 
  | Bbl  of (A.pred list -> A.pred)
  | Bunimpl

(* 
let unop_of_cilUOp = function
  | Cil.Neg ->  A.UnaryMinus
  | Cil.BNot -> A.BitNot
  | Cil.LNot -> A.Not
*)

let op_of_cilBOp = function
  | Cil.PlusA   -> Bop A.Plus    
  | Cil.MinusA  -> Bop A.Minus
  | Cil.MinusPP -> Bop A.Minus
  | Cil.Mult    -> Bop A.Times
  | Cil.Div     -> Bop A.Div  

  | Cil.IndexPI              
  | Cil.PlusPI  -> Bpop A.Plus
  | Cil.MinusPI -> Bpop A.Minus
 
  | Cil.Lt      -> Brl A.Lt   
  | Cil.Gt      -> Brl A.Gt   
  | Cil.Le      -> Brl A.Le   
  | Cil.Ge      -> Brl A.Ge   
  | Cil.Eq      -> Brl A.Eq   
  | Cil.Ne      -> Brl A.Ne  

  | Cil.LOr     -> Bbl A.pOr   
  | Cil.LAnd    -> Bbl A.pAnd

  | Cil.Mod     -> Bop A.Mod
  | Cil.Shiftlt   
  | Cil.Shiftrt   
  | Cil.BAnd                         
  | Cil.BXor                         
  | Cil.BOr     -> Bunimpl

let expr_of_var v =
  A.eVar (Sy.of_string v.Cil.vname)

let expr_of_lval ((lh, _) as lv) = match lh with
  | Cil.Var v when not v.Cil.vglob ->
      expr_of_var v
  | Cil.Var v when v.Cil.vglob ->
      Pretty.printf "Trying to convert global %a to expr\n\n" Cil.d_lval lv;
      assertf "Trying to convert global %s to expr" v.Cil.vname
      (*
      halt <| Errormsg.error "Trying to convert global %a to expr\n\n" Cil.d_lval lv
      *)
  | _ ->
      let _ = Errormsg.error "Unimplemented expr_of_lval: %a" Cil.d_lval lv in 
      assertf "TBD: CilInterface.expr_of_lval"

      
(* API *)
let stride_of_cilexp = Cil.typeOf <+> Cil.unrollType <+> CilMisc.ptrRefType <+> CilMisc.bytesSizeOf


(* convert_cilexp : (unit -> expr) -> Cil.exp -> exp_or_pred *)
let rec convert_cilexp  = function
  | Cil.Const c -> 
      E (expr_of_cilcon c) 
  | Cil.SizeOf t ->
      E (A.eCon (A.Constant.Int (CilMisc.bytesSizeOf t)))
  | Cil.Lval lv -> 
      E (expr_of_lval lv)
  | Cil.StartOf lv ->
      E (expr_of_lval lv)
  | Cil.UnOp (Cil.LNot, e, _) ->
      P (A.pNot (pred_of_cilexp e)) 
  | Cil.BinOp (op, e1, e2, _) -> 
      convert_cilbinexp (op, e1, e2)
  (* any casts go somewhere around HERE *)
  | Cil.CastE (Cil.TInt (_,_), e) ->
      let e' = match convert_cilexp e with 
               | E e' -> e' 
               | _ -> assertf "ERROR: CilInterface.convert_cilexp cast on pred!"
      in E (A.eCst (e', A.Sort.t_int))
  | Cil.CastE (_, e) ->
      convert_cilexp e
  | e -> 
      Errormsg.s <| Errormsg.error "Unimplemented convert_cilexp: %a@!@!" Cil.d_exp e

and convert_cilbinexp (op, e1, e2) =
  let convert_args = Misc.map_pair expr_of_cilexp in
  match op_of_cilBOp op with
  | Bop op' ->
      let e1', e2' = convert_args (e1, e2) in
      E (A.eBin (e1', op', e2'))
  | Bpop pop ->
      let e1', e2' = convert_args (e1, e2) in
      let stride   = stride_of_cilexp e1 in
      E (A.eBin (e1', pop,  A.eTim (A.eInt stride, e2')))
  | Brl rel' ->
      let e1', e2' = convert_args (e1, e2) in
      P (A.pAtom (e1', rel', e2'))
  | Bbl f -> 
      let p1', p2' = Misc.map_pair pred_of_cilexp (e1, e2) in
      P (f [p1'; p2'])
  | Bunimpl ->
      failwith "Unimplemented operator"


(* API *)
and pred_of_cilexp e = 
  match convert_cilexp e with
  | E e when e = A.zero -> A.pFalse
  | E e when e = A.one  -> A.pTrue
  | E e                 -> A.pAtom (e, A.Ne, A.zero) 
  | P p                 -> p

(* API *)
and expr_of_cilexp e = 
  let x = Misc.do_catchu convert_cilexp e (fun _ -> Errormsg.error "Skolem Error1 %a \n" Cil.d_exp e)
  in match x with
  | E e -> e
  | P p -> A.eIte (p, A.one, A.zero)

(*****************************************************************************************************)

let is_int_to_uint_cast (ct, e) = 
  match Cil.unrollType ct, Cil.unrollType <| Cil.typeOf e with
  | (Cil.TInt (ik, _), Cil.TInt (ik', _)) 
    when (not (Cil.isSigned ik) && (Cil.isSigned ik')) -> true
  | _ -> false

let catch_convert_exp s e =
  Misc.do_catchu expr_of_cilexp e (fun _ -> Errormsg.error "%s %a \n" s Cil.d_exp e)

(** [reft_of_cilexp vv e] == a refinement predicate of the 
 *  form {v = e} or an overapproximation thereof 
 *  assumes that "e" is a-normalized *)

let reft_of_string vv str =
  A.pAnd [A.pAtom (A.eVar vv, A.Gt, A.zero);
          A.pAtom (FA.eApp_bbegin (A.eVar vv), A.Eq, A.eVar vv);
          A.pAtom (FA.eApp_bend (A.eVar vv),
                   A.Eq,
                   A.eBin (FA.eApp_bbegin (A.eVar vv), A.Plus, A.eInt (String.length str + 1)))]

let reft_of_cilexp vv e =
  match e with
  | Cil.Const (Cil.CStr str) ->
    reft_of_string vv str
  | Cil.CastE (t, Cil.Const (Cil.CStr str)) when Cil.isPointerType t ->
    reft_of_string vv str
  | Cil.Const (Cil.CReal _)
  | Cil.BinOp (_, Cil.Const (Cil.CReal _), _, _)
  | Cil.BinOp (_, _, Cil.Const (Cil.CReal _), _)
  | Cil.UnOp  (_, Cil.Const (Cil.CReal _), _)
  | Cil.CastE (_, Cil.Const (Cil.CReal _)) ->
      (* Cop out when real constants are involved *)
      A.pTrue

  | Cil.CastE (ct, e) when is_int_to_uint_cast (ct, e) ->
      let _  = Errormsg.log "UINTCAST: %a \n" Cil.d_exp e in
      let e' = catch_convert_exp "ag_reft3" e in
        A.pOr [A.pAnd [A.pAtom (e', A.Ge, A.zero); A.pAtom (A.eVar vv, A.Eq, e')];
               A.pAnd [A.pAtom (e', A.Lt, A.zero); A.pAtom (A.eVar vv, A.Gt, A.zero)]]

  | Cil.CastE (_, _)
  | Cil.Const (Cil.CInt64 (_,_,_))
  | Cil.Const (Cil.CChr _)
  | Cil.SizeOf _
  | Cil.Lval _
  | Cil.BinOp (Cil.PlusA, _, _, _) 
  | Cil.BinOp (Cil.MinusA, _, _, _) 
  | Cil.BinOp (Cil.MinusPP, _, _, _) 
  | Cil.BinOp (Cil.Mult, _, _, _) 
  | Cil.BinOp (Cil.IndexPI, _, _, _)
  | Cil.BinOp (Cil.PlusPI, _, _, _)
  | Cil.BinOp (Cil.MinusPI, _, _, _)
  | Cil.BinOp (Cil.Lt, _, _, _)
  | Cil.BinOp (Cil.Gt, _, _, _)
  | Cil.BinOp (Cil.Le, _, _, _)
  | Cil.BinOp (Cil.Ge, _, _, _)
  | Cil.BinOp (Cil.Eq, _, _, _)
  | Cil.BinOp (Cil.Ne, _, _, _)
  | Cil.BinOp (Cil.LOr, _, _, _)
  | Cil.BinOp (Cil.LAnd, _, _, _)
  | Cil.StartOf _ -> 
      (* {v = e} *)
      let e' = Misc.do_catchu expr_of_cilexp e (fun _ -> Errormsg.error "Skolem Error2 %a \n" Cil.d_exp e)
      in A.pAtom (A.eVar vv, A.Eq, e')
  | Cil.BinOp (Cil.Mod, e1, Cil.Const (Cil.CInt64 (i,_,_)), _) ->
      let m = Int64.to_int i in
      let e1' = expr_of_cilexp e1 in
      A.pImp (A.pAtom (A.zero, A.Lt, A.eInt m),
              A.pAtom (A.eVar vv, A.Eq, A.eMod (e1', m)))
  | Cil.BinOp (Cil.Mod, e1, e2, _) ->
      let e1' = expr_of_cilexp e1 in
      let e2' = expr_of_cilexp e2 in
      A.pImp (A.pAtom (A.zero, A.Lt, e2'),
              A.pAtom (A.eVar vv, A.Eq, A.eModExp (e1', e2')))
  (* | Cil.BinOp (Cil.Mod, _, _, _) *)
  | Cil.BinOp (Cil.Div, _, _, _) ->
      (* There are preconditions; see assume_guarantee_reft_of_cilexp *)
      A.pTrue
  | Cil.BinOp (Cil.Shiftlt, e1, e2, _) ->
      (* {0 <= e2 => e1 <= v *)
      let e1' = expr_of_cilexp e1 in
      let e2' = expr_of_cilexp e2 in
      A.pImp (A.pAtom (A.zero, A.Le, e2'), A.pAtom (e1', A.Le, A.eVar vv))

  | Cil.BinOp (Cil.Shiftrt, e1, e2, _) ->
      (* {0 <= e2 => e1 >= v *)
      let e1' = expr_of_cilexp e1 in
      let e2' = expr_of_cilexp e2 in
      A.pImp (A.pAtom (A.zero, A.Le, e2'), A.pAtom (e1', A.Ge, A.eVar vv))

  | Cil.UnOp (Cil.Neg, e1, _) ->
      (* {v = 0 - e1} *)
      let e1' = expr_of_cilexp e1 in
      A.pAtom (A.eVar vv, A.Eq, A.eBin (A.zero, A.Minus, e1'))

  | Cil.UnOp (Cil.LNot, e1, _) ->
      (* {v = (e1 = 0 ? 1 : 0)} *)
      let e1' = expr_of_cilexp e1 in 
      A.pAtom (A.eVar vv, A.Eq, (A.eIte (A.pAtom (e1', A.Eq, A.zero), A.one, A.zero)))

  | Cil.BinOp (Cil.BAnd, _, _, _) 
  | Cil.BinOp (Cil.BXor, _, _, _)  
  | Cil.BinOp (Cil.BOr , _, _, _) 
  | Cil.UnOp  (Cil.BNot, _, _) ->
      (* Cop out, for now *)
      let _ = Errormsg.warn "Unhandled operator: %a \n" Cil.d_exp e in
      A.pTrue

  | Cil.AddrOf (Cil.Var v, Cil.NoOffset) when CM.is_fun v ->
      A.pAnd [A.pAtom (A.eVar vv, A.Gt, A.zero);
              A.pAtom (FA.eApp_bbegin (A.eVar vv), A.Eq, A.eVar vv);
              A.pAtom (A.eVar vv, A.Lt, FA.eApp_bend (A.eVar vv))]

  | e -> 
      Errormsg.s <| Errormsg.error "Unimplemented reft_of_cilexp: %a@!@!" Cil.d_exp e 



(** [assume_guarantee_reft_of_cilexp vv e] == returns a (ap, gp) option where
 *  ap is an extra assumption that must hold for e to execute safely,
 *  gp is an extra guarantee about the result of evaluating e. 
 *  Assumes that "e" is a-normalized *)

let assume_guarantee_reft_of_cilexp vv e = match e with
  | Cil.BinOp (Cil.PlusPI, e1, e2, _)
  | Cil.BinOp (Cil.IndexPI, e1, e2, _) ->
      let e1' = catch_convert_exp "ag_reft1" e1 in
      let e2' = catch_convert_exp "ag_reft2" e2 in
      let ap  = A.pAtom (A.zero, A.Le, e2') in  
      let gp  = A.pAtom (e1',    A.Le, A.eVar vv) in 
      Some (ap, gp)

  | Cil.BinOp (Cil.Div, e1, e2, _) ->
      let ap  = A.pAtom (expr_of_cilexp e2, A.Ne, A.zero) in
      let e'  = Misc.do_catchu expr_of_cilexp e (fun _ -> Errormsg.error "Skolem Error2 %a \n" Cil.d_exp e) in
      let gp  = A.pAtom (A.eVar vv, A.Eq, e') in
      Some (ap, gp)

  | Cil.BinOp (Cil.Mod, e1, e2, _) -> 
      (* {0 <= v < (abs e2) } *)
      let e2'    = expr_of_cilexp e2 in
      let abse2' = A.eIte (A.pAtom (A.zero, A.Le, e2'), e2', A.eBin (A.zero, A.Minus, e2')) in
      let ap     = A.pAtom (e2', A.Ne, A.zero) in
      let gp     = A.pAnd [A.pAtom (A.zero, A.Le, A.eVar vv); A.pAtom (A.eVar vv, A.Lt, abse2')] in
      Some (ap, gp)

  | _ -> None
  

(* API *)
let reft_of_cilexp vv e = 
  let gp = reft_of_cilexp vv e in
  match assume_guarantee_reft_of_cilexp vv e with
  | None           -> None, gp
  | Some (ap, gp') -> Some ap, A.pAnd [gp; gp']

let foldGlobalsIf process cil f = 
  Cil.foldGlobals cil (fun acc g -> if process g then f acc g else acc)

let iterGlobalsIf process cil f = 
  Cil.iterGlobals cil (fun g -> if process g then f g )
