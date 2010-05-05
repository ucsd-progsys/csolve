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

open Misc.Ops

(****************************************************************)
(********************* Constants ********************************)
(****************************************************************)

let exp_of_cilcon skolem = function
  | Cil.CInt64 (i, _, _) -> 
      A.eCon (A.Constant.Int (Int64.to_int i))
  | Cil.CChr c ->
      A.eCon (A.Constant.Int (Char.code c))
  | Cil.CReal _ ->
      skolem ()
  | Cil.CStr s ->
      skolem ()
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

  | Cil.Mod       
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
      halt <| Errormsg.error "Trying to convert global %a to expr\n\n" Cil.d_lval lv
  | _ ->
      let _ = Errormsg.error "Unimplemented expr_of_lval: %a" Cil.d_lval lv in 
      assertf "TBD: CilInterface.expr_of_lval"

(* convert_cilexp : (unit -> expr) -> Cil.exp -> exp_or_pred *)
let rec convert_cilexp skolem = function
  | Cil.Const c -> 
      E (exp_of_cilcon skolem c)
  | Cil.SizeOf t ->
      E (A.eCon (A.Constant.Int (CilMisc.bytesSizeOf t)))
  | Cil.Lval lv -> 
      E (expr_of_lval lv)
  | Cil.StartOf lv ->
      E (expr_of_lval lv)
  | Cil.UnOp (Cil.LNot, e, _) ->
      P (A.pNot (pred_of_cilexp skolem e)) 
  | Cil.BinOp (op, e1, e2, _) -> 
      convert_cilbinexp skolem (op, e1, e2)
  | Cil.CastE (Cil.TInt (_,_), e) ->
      let e' = match convert_cilexp skolem e with E e' -> e' | _ -> assertf "ERROR: CilInterface.convert_cilexp cast on pred!"
      in E (A.eCst (e', A.Sort.t_int))
  | Cil.CastE (_, e) ->
      convert_cilexp skolem e
  | e -> 
      Errormsg.error "Unimplemented convert_cilexp: %a@!@!" Cil.d_exp e;
      assertf "crash"

and convert_cilbinexp skolem (op, e1, e2) =
  let convert_args = Misc.map_pair (expr_of_cilexp skolem) in
  match op_of_cilBOp op with
  | Bop op' ->
      let e1', e2' = convert_args (e1, e2) in
      E (A.eBin (e1', op', e2'))
  | Bpop pop ->
      let e1', e2' = convert_args (e1, e2) in
      let stride   = Cil.typeOf e1 |> Cil.unrollType |> CilMisc.ptrRefType |> CilMisc.bytesSizeOf in
      E (A.eBin (e1', pop, A.eBin (A.eCon (A.Constant.Int stride), A.Times, e2')))
  | Brl rel' ->
      let e1', e2' = convert_args (e1, e2) in
      P (A.pAtom (e1', rel', e2'))
  | Bbl f -> 
      let p1', p2' = Misc.map_pair (pred_of_cilexp skolem) (e1, e2) in
      P (f [p1'; p2'])
  | Bunimpl ->
      E (skolem ())

(* API *)
and pred_of_cilexp skolem e = 
  match convert_cilexp skolem e with
  | E e when e = A.zero -> A.pFalse
  | E e when e = A.one  -> A.pTrue
  | E e                 -> A.pAtom (e, A.Ne, A.zero) 
  | P p                 -> p

(* API *)
and expr_of_cilexp skolem e = 
  match convert_cilexp skolem e with
  | E e -> e
  | P p -> A.eIte (p, A.one, A.zero)

(*****************************************************************************************************)

let bogusk = fun _ -> failwith "CI: bogus skolem"

(** [reft_of_cilexp vv e] == a refinement predicate of the form {v = e} or an overapproximation thereof 
 *  assumes that "e" is a-normalized *)
let rec reft_of_cilexp vv e =
  match e with
  | Cil.Const (Cil.CStr str) ->
      (* pmr: Can and should do more here - vv = block start, length = len (str) *)
      A.pAnd [A.pAtom (A.eVar vv, A.Ne, A.zero)]

  | Cil.Const (Cil.CReal _)
  | Cil.BinOp (_, Cil.Const (Cil.CReal _), _, _)
  | Cil.BinOp (_, _, Cil.Const (Cil.CReal _), _)
  | Cil.UnOp  (_, Cil.Const (Cil.CReal _), _) ->
      (* Cop out when real constants are involved *)
      A.pTrue

  | Cil.CastE (_, _)
  | Cil.Const (Cil.CInt64 (_,_,_))
  | Cil.Const (Cil.CChr _)
  | Cil.SizeOf _
  | Cil.Lval _
  | Cil.BinOp (Cil.PlusA, _, _, _) 
  | Cil.BinOp (Cil.MinusA, _, _, _) 
  | Cil.BinOp (Cil.MinusPP, _, _, _) 
  | Cil.BinOp (Cil.Mult, _, _, _) 
  | Cil.BinOp (Cil.Div, _, _, _)
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
  | Cil.BinOp (Cil.LAnd, _, _, _) -> 
      (* {v = e} *)
      A.pAtom (A.eVar vv, A.Eq, expr_of_cilexp bogusk e)
  
  | Cil.BinOp (Cil.Mod, e1, e2, _) -> 
      (* {0 <= v < (abs e2) } *)
      let e2'    = expr_of_cilexp bogusk e2 in
      let abse2' = A.eIte (A.pAtom (A.zero, A.Le, e2'), e2', A.eBin (A.zero, A.Minus, e2')) in
      A.pAnd [A.pAtom (A.zero, A.Le, A.eVar vv); A.pAtom (A.eVar vv, A.Lt, abse2')]
  
  | Cil.BinOp (Cil.Shiftlt, e1, e2, _) ->
      (* {0 <= e2 => e1 <= v *)
      let e1' = expr_of_cilexp bogusk e1 in
      let e2' = expr_of_cilexp bogusk e2 in
      A.pImp (A.pAtom (A.zero, A.Le, e2'), A.pAtom (e1', A.Le, A.eVar vv))

  | Cil.BinOp (Cil.Shiftrt, e1, e2, _) ->
      (* {0 <= e2 => e1 >= v *)
      let e1' = expr_of_cilexp bogusk e1 in
      let e2' = expr_of_cilexp bogusk e2 in
      A.pImp (A.pAtom (A.zero, A.Le, e2'), A.pAtom (e1', A.Ge, A.eVar vv))

  | Cil.UnOp (Cil.Neg, e1, _) ->
      (* {v = 0 - e1} *)
      let e1' = expr_of_cilexp bogusk e1 in 
      A.pAtom (A.eVar vv, A.Eq, A.eBin (A.zero, A.Minus, e1'))

  | Cil.UnOp (Cil.LNot, e1, _) ->
      (* {v = (e1 = 0 ? 1 : 0)} *)
      let e1' = expr_of_cilexp bogusk e1 in 
      A.pAtom (A.eVar vv, A.Eq, (A.eIte (A.pAtom (e1', A.Eq, A.zero), A.one, A.zero)))

  | Cil.BinOp (Cil.BAnd, _, _, _) 
  | Cil.BinOp (Cil.BXor, _, _, _)  
  | Cil.BinOp (Cil.BOr , _, _, _) 
  | Cil.UnOp  (Cil.BNot, _, _) ->
      (* Cop out, for now *)
      let _ = Errormsg.warn "Unhandled operator: %a \n" Cil.d_exp e in
      A.pTrue

  | e -> 
      Errormsg.error "Unimplemented reft_cilexp: %a@!@!" Cil.d_exp e;
      assertf "crash"
