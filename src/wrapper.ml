(*******************************************************************)
(**************** Wrapper between LiquidC and Fixpoint *************)
(*******************************************************************)

type cilenv  = (Cil.varinfo * C.reft) SM.t

type cilcstr = cilenv * (int * bool) list * C.reft * C.reft * Cil.location

let cilenv_add v r cenv =
  SM.add v.vname (v, r) cenv

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let fresh ty : C.reft =
  match ty with
  | TInt _ ->
      C.make_reft (Sy.value_var So.Int) So.Int [C.Kvar ([], fresh_kvar ())]
  | _      -> 
      asserts false "TBD: Consgen.fresh"

(* Cil.typ -> Ast.Sort.t *)

let sort_of_typ = function
  | TInt _ -> 
      Int
  | _ -> 
      asserts false "TBD: Wrapper.sort_of_typ"

(* Cil.expr -> Ast.expr  *)

let con_of_cilcon = function
  | CInt64 (i, _, _) -> Int64.to_int i
  | _ -> asserts false "TBD: Wrapper.con_of_cilcon"

let expr_of_lval (lh, _) = match lh with
  | Var v -> v.vname |> Sy.of_string |> Ast.eVar
  | _ -> asserts false "TBD: Wrapper.expr_of_lval"

let expr_of_cilexpr = function
  | Const c -> Ast.eCon (con_of_cilcon c)
  | Lval lv -> expr_of_lval lv  
  | _ -> asserts false "TBD: Wrapper.expr_of_cilexpr"

let pred_of_cilexpr = function
  | _   -> asserts false "TBD: Wrapper.pred_of_cilexpr"

(* creating refinements *)

let t_single (e : Cil.expr) : C.reft =
  let ty = Cil.typeOf e in
  let so = sort_of_typ ty in
  let vv = Sy.value_var so in
  let p  = A.pAtom (A.eVar vv, A.Eq, expr_of_cilexpr e) in
  (vv, so, [(C.Conc p)])

let expr_of_var v = 
  Lval ((Var v), NoOffset)

let t_var (v : Cil.varinfo) : C.reft =
  v |> expr_of_var |> t_single 

let expand_guard ifs ibs =
  ibs  
  |> List.map (fun (i,b) -> match ifs.(i) with 
               | Some (e,_,_) -> 
                   let p  = pred_of_cilexpr e in
                   let p' = if b then p else A.pNot p in
                   (i, p') 
               | _ -> 
                   asserts false "ERROR: expand_guard")
  |> A.pAnd

let mk_cilcstr cenv ibs lhst rhst loc = 
  (cenv, ibs, lhst, rhst, loc)

let cstr_of_cilcstr sci p (cenv, ibs, r1, r2, _) = 
  let gp  = expand_guard  sci.ST.ifs ibs in
  let env = SM.map snd cenv in
  C.make_t env (pAnd [p; gp]) r1 r2 
