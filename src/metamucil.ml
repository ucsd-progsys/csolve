(*module Template = struct
  type psub = (string * Expression.t) list
  type kvar = int 
  type refn = Conc of Predicate.t | Kvar of kvar * psub
  type soln = Predicate.t list IM.t 

  type uty  = typ
  and  t    = uty * refn
  type env  = (t * varinfo)  SM.t

  let d_refn () = function
    | Conc p    -> Pretty.dprintf "%a" Predicate.d_pred p
    | Kvar (i,_) -> Pretty.dprintf "k_%d" i 

  let d_tplt () (u, r) = 
    Pretty.dprintf "{V: %a | %a}" d_type u d_refn r 

  let value_var u : Cil.varinfo =
    let vn = Pretty.sprint ~width:80 (Pretty.dprintf "VV_%a" d_type u) in
    makeGlobalVar vn u

  let mk_ty u r = (u, r)

  let get_refn (t: t) : refn = snd t

  let get_kvars (t: t) : kvar list =
    match get_refn t with Kvar (k, _) -> [k] | _ -> []

  let t_const e : t =
    let u  = typeOf e in
    let vv = Lval ((Var (value_var u)), NoOffset) in
    let r  = Conc (Predicate.mk_eq vv e) in
    mk_ty u r 

  let t_var (g:env) (v:varinfo) : t =
    let vn = v.vname in
    try fst (SM.find vn g) with Not_found -> 
      failwith ("t_var: unknown var"^vn)

  let t_exp (g:env) (e:Expression.t) : t = 
    match e with
    | Const _   | UnOp _  | BinOp _ -> 
        t_const e  
    | _ ->
        raise Unhandled
        *)

module SM = Misc.StringMap
module P = Ast.Predicate

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
    (vv, vvt, Conc [P.mk_eq vv e])

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
  | Var v -> P.eVar v
  | _ -> assert false

let expr_of_cilexpr = function
  | Const c -> P.eCon (con_of_cilcon c)
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
  P.bAtom (expr_of_cilexpr e1) Ast.Eq (expr_of_cilexpr e2)
