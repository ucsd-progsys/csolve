module Ix = Index
module As = Ast.Symbol
module Ac = Ast.Constant  
module Asm = As.SMap  
module Sct = ScalarCtypes
module F   = FixConstraint
module FAI = FixAstInterface

open Ix  
open Misc.Ops  

type bind = Ix.t
type t    = Ix.t Asm.t

let empty = Asm.empty

let read sol =
  fun k -> 
    if Asm.mem k sol
    then
      match Asm.find k sol with
	| IBot -> [Ast.pFalse]
	| IInt i -> [Ast.pAtom (Ast.eVar k, Ast.Eq, Ast.eCon (Ac.Int i))]
	| _ -> [Ast.pTrue]
    else
      [Ast.pFalse]

let read_bind sol k = if Asm.mem k sol then Asm.find k sol else IBot

let top sol xs =
  let xsTop = List.map (fun x -> x, top) xs in
  let xsMap = Asm.of_list xsTop in
    Asm.extend xsMap sol

let rec index_of_pred env solution sym t (pred,_) =
  match pred with
    | Ast.True -> Ix.top
    | Ast.False -> IBot
    | Ast.Or ps -> List.map (index_of_pred env solution sym t) ps
                   |> List.fold_left lub IBot
    | Ast.And ps -> List.map (index_of_pred env solution sym t) ps
                   |> List.fold_left glb Ix.top
    | Ast.Atom ((Ast.Var vv, _),Ast.Eq,e)
    | Ast.Atom (e,Ast.Eq,(Ast.Var vv, _)) when vv = sym ->
	index_of_expr env solution t e
    | Ast.Atom ((Ast.Var vv, _),r,e) when vv = sym ->
	let eVal = index_of_expr env solution t e in
	  begin match r with
	    | Ast.Gt -> gt eVal
	    | Ast.Ge -> ge eVal
	    | Ast.Lt -> lt eVal
	    | Ast.Le -> le eVal
	  end
    | Ast.Atom (e,r,(Ast.Var vv, _)) when vv = sym ->
	let eVal = index_of_expr env solution t e in
	  begin match r with
	    | Ast.Gt -> lt eVal
	    | Ast.Ge -> le eVal
	    | Ast.Lt -> gt eVal
	    | Ast.Le -> ge eVal
	  end
    | Ast.Bexp e -> (* shortcut for now *)
	begin match fst e with
	  | Ast.Con (Ac.Int n) -> of_int n
	  | _ -> Ix.top
	end
    | _ -> Ix.top
and index_of_expr env solution t expr =
  match fst expr with
    | Ast.Con (Ast.Constant.Int n) ->
	if Ast.Sort.is_int t then IInt n else IBot
    | Ast.Var sym ->
	begin match F.lookup_env env sym with
	  | None -> IBot
	  | Some (sym, sort, refas) when sort = t -> (* questionable? *)
	      List.fold_left glb Ix.top
		(List.map (index_of_refa env solution sym t) refas)
	end
    | Ast.Bin (e1, oper, e2) ->
	let e1v = index_of_expr env solution t e1 in
	let e2v = index_of_expr env solution t e2 in
	  begin match oper with
	    | Ast.Plus  -> plus  e1v e2v
	    | Ast.Minus -> minus e1v e2v
	    | Ast.Times -> mult  e1v e2v
	    | Ast.Div   -> div   e1v e2v
	    | Ast.Mod   -> Ix.top
	  end
    | Ast.App (sym,e)
	when expr = Ast.eApp (* (FAI.eApp_bbegin e) -> *) (As.of_string "BLOCK_BEGIN", e) -> IInt 0 
    | Ast.Cst (e,sort) -> index_of_expr env solution t e
    | _ -> Ix.top
and index_of_preds env sol v t preds = 
  List.map (index_of_pred env sol v t) preds|> List.fold_left glb Ix.top
and index_of_refa env sol v t r = match r with
  | F.Kvar (_, k) -> read_bind sol k
  | F.Conc pred -> (* index_of_pred env sol v pred *)
      Sct.index_of_pred
	(begin fun v p ->
	   [index_of_preds env sol v t p] end::Sct.data_index_of_pred_funs) v pred
and index_of_reft env solution (v, t, refas) =
  List.fold_left glb Ix.top (List.map (index_of_refa env solution v t) refas)

let refine sol c =
  let rhs = F.rhs_of_t c in
  let lhsVal = index_of_reft (F.env_of_t c) sol (F.lhs_of_t c) in
  let refineK sol k =
    let oldK = if Asm.mem k sol then Asm.find k sol else IBot in
    let newK = widen oldK lhsVal in
(*    let _ = Printf.printf "lhsVal: %s\noldK: %s newK: %s from c: \n" (repr lhsVal) (repr oldK) (repr newK) in *)
    let _ = Printf.printf "Cons: %s" (F.to_string c) in
      if oldK = newK then (false, sol) else (true, Asm.add k newK sol)
  in
    List.fold_left
      begin fun p k -> match p,k with
	| (_, sol), (_, sym) -> refineK sol sym
	| _ -> (false, sol)
      end
      (false, sol) (F.kvars_of_reft rhs)

let unsat sol c =
  (* Make sure that lhs <= k for each k in rhs *)
  let rhsKs = F.rhs_of_t c |> F.kvars_of_reft  in
  let lhsVal = index_of_reft (F.env_of_t c) sol (F.lhs_of_t c) in
  let onlyK v = match v with (* true if the constraint is unsatisfied *)
    | sub, sym ->
	if Asm.mem sym sol then
	  not (is_subindex lhsVal (Asm.find sym sol))
	else
	  true
  in
    List.map onlyK rhsKs |> List.fold_left (&&) true
	
let create cfg =
  let replace v = IBot in
    Asm.map replace cfg.Config.bm
	
let print ppf sol =
  let pf key value =
    Pretty.printf "%s |-> %a\n" (As.to_string key) d_index value in
  let _ = Asm.mapi pf sol in
    ()

let print_stats ppf sol =
  ()

let dump sol =
  let pf key value =
    Pretty.printf "%s |-> %a\n" (As.to_string key) d_index value in
  let _ = Asm.mapi pf sol in
    ()

let mkbind qbnds = IBot
(* end *)
  
