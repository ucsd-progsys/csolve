module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  C = Constraint
module Sy = A.Symbol
module So = A.Sort

open Misc.Ops
open Cil


(*******************************************************************)
(**************** Wrapper between LiquidC and Fixpoint *************)
(*******************************************************************)

type cilenv  = (Cil.varinfo * C.reft) SM.t
type cilcstr = cilenv * (int * bool) list * C.reft * C.reft * Cil.location

let ce_empty = SM.empty
let ce_find  = fun v cenv -> SM.find v.vname cenv
let ce_add   = fun v r cenv -> SM.add v.vname (v, r) cenv


let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let fresh ty : C.reft =
  match ty with
  | TInt _ ->
      C.make_reft (Sy.value_variable So.Int) So.Int [C.Kvar ([], fresh_kvar ())]
  | _      -> 
      asserts false "TBD: Consgen.fresh";
      assert false

(* Cil.typ -> Ast.Sort.t *)

let sort_of_typ = function
  | TInt _ -> 
      So.Int
  | _ -> 
      asserts false "TBD: Wrapper.sort_of_typ";
      assert false

(* Cil.expr -> Ast.expr  *)

let con_of_cilcon = function
  | CInt64 (i, _, _) -> 
      A.Constant.Int (Int64.to_int i)
  | _ -> 
      asserts false "TBD: Wrapper.con_of_cilcon";
      assert false

let expr_of_lval (lh, _) = match lh with
  | Var v -> 
      v.vname |> Sy.of_string |> Ast.eVar
  | _ -> 
      asserts false "TBD: Wrapper.expr_of_lval"; 
      assert false

let expr_of_cilexp = function
  | Const c -> 
      Ast.eCon (con_of_cilcon c)
  | Lval lv -> 
      expr_of_lval lv  
  | _ -> 
      asserts false "TBD: Wrapper.expr_of_cilexp"; 
      assert false

let pred_of_cilexp = function
  | _   -> 
      asserts false "TBDNOW: Wrapper.pred_of_cilexp"; 
      assert false

(* creating refinements *)

let t_single (e : Cil.exp) : C.reft =
  let ty = Cil.typeOf e in
  let so = sort_of_typ ty in
  let vv = Sy.value_variable so in
  let p  = A.pAtom (A.eVar vv, A.Eq, expr_of_cilexp e) in
  (vv, so, [(C.Conc p)])

let expr_of_var v = 
  v.vname |> Sy.of_string |> A.eVar

let t_var (v : Cil.varinfo) : C.reft =
  t_single (Lval ((Var v), NoOffset))

let expand_guard ifs ibs =
  ibs  
  |> List.map (fun (i,b) -> match ifs.(i) with 
               | Some (e,_,_) -> 
                   let p  = pred_of_cilexp e in
                   if b then p else (A.pNot p)
               | _ -> 
                   asserts false "ERROR: expand_guard";
                   assert false)
  |> A.pAnd

let mk_cilcstr cenv ibs lhst rhst loc = 
  (cenv, ibs, lhst, rhst, loc)

let env_of_cilenv cenv = 
  SM.fold 
    (fun x (_,r) env -> Sy.SMap.add (Sy.of_string x) r env) 
    cenv
    Sy.SMap.empty

let cstr_of_cilcstr sci p (cenv, ibs, r1, r2, _) = 
  let env = env_of_cilenv cenv in
  let gp  = expand_guard  sci.ST.ifs ibs in
  C.make_t env (A.pAnd [p; gp]) r1 r2 
