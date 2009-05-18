module SM = Misc.StringMap
module P = Ast.Predicate
module C = Constraint

open Cil

type cilenv = (Cil.typ * C.refa * Cil.varinfo) SM.t

let value_var = "AA_"

(* type extraction *)

let ciltyp_of_var (g:cilenv) (v:varinfo) =
  let vn = v.vname in
  try fst (SM.find vn g) with Not_found -> 
    failwith ("t_var: unknown var "^vn)

let mk_const_reft e =
  let vvt  = sort_of_type (typeOf e) in
  let vv = Ast.eVar value_var in
    (vv, vvt, C.Conc [P.mk_eq vv e])

(* cil -> solver translation *)

(* types *)

let sort_of_typ = function
  | TInt (k, attrs) -> Int
  | _ -> assert false

(* expressions *)

let con_of_cilcon = function
  | CInt64 (i, _, _) -> Int64.to_int i
  | _ -> assert false

let expr_of_lval (lh, _) = match lh with
  | Var v -> Ast.eVar v
  | _ -> assert false

let expr_of_cilexpr = function
  | Const c -> Ast.eCon (con_of_cilcon c)
  | Lval lv -> expr_of_lval lv  
  | _ -> assert false

(* environments *)

let env_of_cilenv cm =
  let add_reft (ty, ref, vinf) rm = 
    let vn = vinf.vname in
    SM.add vn (vn, sort_of_typ ty, [ref]) rm in
  SM.fold add_ret SM.empty cm

(* constraint constructor *)

let mk_constr =
  let id = ref 0 in
  fun cilenv grd lhs rhs ->
    (env_of_cilenv cilenv, grd, lhs, rhs, (Some (incr id; !id)))

(* predicate constructors *)

let mk_eq e1 e2 =
  Ast.bAtom (expr_of_cilexpr e1) Ast.Eq (expr_of_cilexpr e2)
