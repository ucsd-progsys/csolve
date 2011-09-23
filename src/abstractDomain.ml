module Ix = Index
module As = Ast.Symbol
module Ac = Ast.Constant  
module Asm = As.SMap  
module Sct = ScalarCtypes
module F   = FixConstraint  

open Ix  
open Misc.Ops  

module type AbstractDomain = Config.DOMAIN
(*module AbstractDomain = struct *)
  (* All definitions below are placeholders *)
  (* First bit of TODO, just to get the solver to go all the way through
     without really doing anything:
       Define t, bind
       Define create, empty, and top as trivially as possible
         (map every kvar to bot)
       Define printing functions
       Define unsat to always return false
       Define refine to do nothing
  *)
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

(*  let top = top      *)

let top sol xs =
  let xsTop = List.map (fun x -> x, top) xs in
  let xsMap = Asm.of_list xsTop in
    Asm.extend xsMap sol

let rec index_of_pred env solution sym (pred,_) =
  match pred with
    | Ast.True -> Ix.top
    | Ast.False -> IBot
    | Ast.Or ps -> List.map (index_of_pred env solution sym) ps
                   |> List.fold_left lub IBot
    | Ast.And ps -> List.map (index_of_pred env solution sym) ps
                   |> List.fold_left glb Ix.top
    | Ast.Atom ((Ast.Var vv, _),Ast.Eq,e)
    | Ast.Atom (e,Ast.Eq,(Ast.Var vv, _)) when vv = sym ->
	index_of_expr env solution e
    | Ast.Atom ((Ast.Var vv, _),r,e) when vv = sym ->
	let eVal = index_of_expr env solution e in
	  begin match r with
	    | Ast.Gt -> gt eVal
	    | Ast.Ge -> ge eVal
	    | Ast.Lt -> lt eVal
	    | Ast.Le -> le eVal
	  end
    | Ast.Atom (e,r,(Ast.Var vv, _)) when vv = sym ->
	let eVal = index_of_expr env solution e in
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
and index_of_expr env solution expr =
  match fst expr with
    | Ast.Con (Ast.Constant.Int n) ->
	IInt n
    | Ast.Var sym ->
	begin match F.lookup_env env sym with
	  | None -> IBot
	  | Some (sym, sort, refas) when Ast.Sort.is_int sort ->
	      List.fold_left glb Ix.top
		(List.map (index_of_refa env solution sym) refas)
	end
    | Ast.Bin (e1, oper, e2) ->
	let e1v = index_of_expr env solution e1 in
	let e2v = index_of_expr env solution e2 in
	  begin match oper with
	    | Ast.Plus  -> plus  e1v e2v
	    | Ast.Minus -> minus e1v e2v
	    | Ast.Times -> mult  e1v e2v
	    | Ast.Div   -> div   e1v e2v
	    | Ast.Mod   -> Ix.top
	  end
    | Ast.App (sym,e)
	when expr = Ast.eApp (As.of_string "BLOCK_BEGIN", e) -> IInt 0
    | Ast.Cst (e,sort) -> index_of_expr env solution e
    | _ -> Ix.top
and index_of_preds env sol v preds = 
  List.map (index_of_pred env sol v) preds|> List.fold_left glb Ix.top
and index_of_refa env sol v r = match r with
  | F.Kvar (_, k) -> read_bind sol k
  | F.Conc pred -> (* index_of_pred env sol v pred *)
      Sct.index_of_pred
	(begin fun v p ->
	   [index_of_preds env sol v p] end::Sct.data_index_of_pred_funs) v pred
and index_of_reft env solution (v, t, refas) =
(*  let f i1 i2 = let _ = Printf.printf "glb of %s %s = %s\n" (repr i1) (repr i2) (repr (glb i1 i2)) in glb i1 i2 in
  List.fold_left f Ix.top (List.map (index_of_refa env solution v) refas) *)
  List.fold_left glb Ix.top (List.map (index_of_refa env solution v) refas)

let refine sol c =
  let rhs = F.rhs_of_t c in
  let lhsVal = index_of_reft (F.env_of_t c) sol (F.lhs_of_t c) in
  let refineK sol k =
    let oldK = if Asm.mem k sol then Asm.find k sol else IBot in
    let newK = widen oldK lhsVal in
(*    let _ = Printf.printf "lhsVal: %s\noldK: %s newK: %s from c: \n" (repr lhsVal) (repr oldK) (repr newK) in
    let _ = Printf.printf "%s" (F.to_string c) in *)
      if oldK = newK then (false, sol) else (true, Asm.add k newK sol)
  in
    List.fold_left
      begin fun p k -> match p,k with
	| (_, sol), (_, sym) -> refineK sol sym
	| _ -> (false, sol)
      end
      (false, sol) (F.kvars_of_reft rhs)

let refine sol c =
  (*    let _ = Pretty.printf ">>> Before Refine:\n" in
	let _ = dump sol in *)
  let c,s = refine sol c in (*
			      let _ = Pretty.printf "<<< After Refine:\n" in
			      let _ = dump s in *)
    c,s



let unsat sol c =
  (* Make sure that lhs <= k for each k in rhs *)
(*  let _ = Printf.printf "***unsat on %s\n" (F.to_string c) in *)
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
  
