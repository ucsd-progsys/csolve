module C = FixConstraint
module P = Ast.Predicate
module Sy = Ast.Symbol

open Misc.Ops

let neg_brel = function 
  | Ast.Eq -> Ast.Ne
  | Ast.Ne -> Ast.Eq
  | Ast.Gt -> Ast.Le
  | Ast.Ge -> Ast.Lt
  | Ast.Lt -> Ast.Ge
  | Ast.Le -> Ast.Gt

let rec push_neg ?(neg=false) ((p, _) as pred) =
  match p with
    | Ast.True -> if neg then Ast.pFalse else pred
    | Ast.False -> if neg then Ast.pTrue else pred
    | Ast.Bexp _ -> if neg then Ast.pNot pred else pred
    | Ast.Not p -> push_neg ~neg:(not neg) p
    | Ast.Imp (p, q) -> 
	if neg then Ast.pAnd [push_neg p; push_neg ~neg:true q]
	else Ast.pImp (push_neg p, push_neg q)
    | Ast.Forall (qs, p) -> 
	let pred' = Ast.pForall (qs, push_neg ~neg:false p) in
	  if neg then Ast.pNot pred' else pred'
    | Ast.And ps -> List.map (push_neg ~neg:neg) ps |> if neg then Ast.pOr else Ast.pAnd
    | Ast.Or ps -> List.map (push_neg ~neg:neg) ps |> if neg then Ast.pAnd else Ast.pOr
    | Ast.Atom (e1, brel, e2) -> if neg then Ast.pAtom (e1, neg_brel brel, e2) else pred

(* Andrey: TODO flatten nested conjunctions/disjunctions *)
let rec simplify_pred ((p, _) as pred) =
  match p with
    | Ast.Not p -> Ast.pNot (simplify_pred p)
    | Ast.Imp (p, q) -> Ast.pImp (simplify_pred p, simplify_pred q) 
    | Ast.Forall (qs, p) -> Ast.pForall (qs, simplify_pred p)
    | Ast.And ps -> 
	let ps' = List.map simplify_pred ps |> List.filter (fun p -> not(P.is_tauto p)) in
	  if List.mem Ast.pFalse ps' then Ast.pFalse else
	    begin
	      match ps' with
		| [] -> Ast.pTrue
		| [p'] -> p'
		| _ :: _ -> Ast.pAnd ps'
	    end
    | Ast.Or ps -> 
	let ps' = List.map simplify_pred ps in
	  if List.exists P.is_tauto ps' then Ast.pTrue else 
	    begin
	      match ps' with
		| [] -> Ast.pFalse
		| [p'] -> p'
		| _ :: _ -> Ast.pOr ps'
	    end
    | _ -> pred

let rec defs_of_pred edefs pdefs ((p, _) as pred) = 
  match p with
    | Ast.Atom ((Ast.Var v, _), Ast.Eq, e) when not(P.is_tauto pred) -> Sy.SMap.add v e edefs, pdefs
    | Ast.And [Ast.Imp ((Ast.Bexp (Ast.Var v1, _), _), p1), _; 
	       Ast.Imp (p2, (Ast.Bexp (Ast.Var v2, _), _)), _] when v1 = v2 && p1 = p2 && not(P.is_tauto pred) -> 
	edefs, Sy.SMap.add v1 p1 pdefs
    | Ast.And preds -> 
	let edefs', pdefs' = List.fold_left 
	  (fun (edefs_sofar, pdefs_sofar) p ->
	     let edefs'', pdefs'' = defs_of_pred edefs_sofar pdefs_sofar p in
	       edefs'', pdefs''
	  ) (edefs, pdefs) preds in
	  edefs', pdefs'
    | _ -> edefs, pdefs


let some_def_applied = ref false
let rec expr_apply_defs edefs pdefs ((e, _) as expr) = 
  let current_some_def_applied = !some_def_applied in
    some_def_applied := false;
    let expr'' =
      match e with
	| Ast.Con _ -> expr
	| Ast.Var v -> 
	    begin
	      try
		let expr' = Sy.SMap.find v edefs in
		  some_def_applied := true;
		  expr'
	      with Not_found -> expr
	    end
	| Ast.App (v, es) -> 
	    let edefs' = Sy.SMap.remove v edefs in
	      Ast.eApp (v, List.map (expr_apply_defs edefs' pdefs) es)
	| Ast.Bin (e1, op, e2) -> 
	    Ast.eBin (expr_apply_defs edefs pdefs e1, op, expr_apply_defs edefs pdefs e2)
	| Ast.Ite (p, e1, e2) -> 
	    Ast.eIte (pred_apply_defs edefs pdefs p, 
		      expr_apply_defs edefs pdefs e1,
		      expr_apply_defs edefs pdefs e2)
	| Ast.Fld (v, e) -> 
	    let v' = 
	      try
		match Sy.SMap.find v edefs with
		  | (Ast.Var v'', _) -> 
		      some_def_applied := true;
		      v''
		  | _ -> v
	      with Not_found -> v
	    in
	      Ast.eFld (v', expr_apply_defs edefs pdefs e)
    in
      if !some_def_applied then
	let expr''' = expr_apply_defs edefs pdefs expr'' in
	  some_def_applied := current_some_def_applied;
	  expr'''
      else
	begin
	  some_def_applied := current_some_def_applied;
	  expr''
	end
and pred_apply_defs edefs pdefs ((p, _) as pred) =
  let current_some_def_applied = !some_def_applied in
    some_def_applied := false;
    let pred'' =
      match p with
	| Ast.And ps -> List.map (pred_apply_defs edefs pdefs) ps |> Ast.pAnd
	| Ast.Or ps -> List.map (pred_apply_defs edefs pdefs) ps |> Ast.pOr
	| Ast.Not p -> pred_apply_defs edefs pdefs p |> Ast.pNot
	| Ast.Imp (p, q) -> Ast.pImp (pred_apply_defs edefs pdefs p, pred_apply_defs edefs pdefs q)
	| Ast.Bexp (Ast.Var v, _) ->
	    begin
	      try
		let expr' = Sy.SMap.find v edefs in
		  some_def_applied := true;
		  Ast.pBexp expr'
	      with Not_found ->
		try
		  let pred' = Sy.SMap.find v pdefs in
		    some_def_applied := true;
		    pred'
		with Not_found ->
		  pred
	    end
	| Ast.Atom (e1, brel, e2) ->
	    Ast.pAtom (expr_apply_defs edefs pdefs e1, brel, expr_apply_defs edefs pdefs e2)
	| Ast.Forall (qs, p) ->
	    let vs = List.map fst qs in
	    let edefs' = List.fold_left (fun defs v -> Sy.SMap.remove v defs) edefs vs in
	    let pdefs' = List.fold_left (fun defs v -> Sy.SMap.remove v defs) pdefs vs in
	      Ast.pForall (qs, pred_apply_defs edefs' pdefs' p)
	| _ -> pred
    in
      if !some_def_applied then
	let pred''' = pred_apply_defs edefs pdefs pred'' in
	  some_def_applied := current_some_def_applied;
	  pred'''
      else 
	begin
	  some_def_applied := current_some_def_applied;
	  pred''
	end

let subs_apply_defs edefs pdefs subs =
  List.map (fun (s, e) -> s, expr_apply_defs edefs pdefs e) subs

let kvar_apply_defs edefs pdefs (subs, sym) = 
  subs_apply_defs edefs pdefs subs, sym

let preds_kvars_of_reft reft =
  List.fold_left 
    (fun (ps, ks) r ->
       match r with
	 | C.Conc p -> p :: ps, ks
	 | C.Kvar (subs, kvar) -> ps, (subs, kvar) :: ks
    ) ([], []) (C.ras_of_reft reft)

let simplify_subs subs =
  List.filter (fun (s, e) -> not(P.is_tauto (Ast.pAtom (Ast.eVar s, Ast.Eq, e)))) subs

let simplify_kvar (subs, sym) =
  simplify_subs subs, sym

let simplify_t t = 
  let env_ps, pfree_env = 
    Sy.SMap.fold 
      (fun bv reft (ps, env) -> 
	 let vv = C.vv_of_reft reft in
	 let bv_expr = Ast.eVar bv in
	 let sort = C.sort_of_reft reft in
	 let reft_ps, reft_ks = preds_kvars_of_reft reft in
	   (List.rev_append (List.map (fun p -> P.subst p vv bv_expr) reft_ps) ps,
	    if reft_ks = [] then env 
	    else Sy.SMap.add bv (vv, sort, reft_ks) env)
      ) (C.env_of_t t) ([], Sy.SMap.empty) in
  let lhs = C.lhs_of_t t in
  let lhs_ps, lhs_ks = preds_kvars_of_reft lhs in
  let body_pred = Ast.pAnd (C.grd_of_t t :: List.rev_append lhs_ps env_ps) in
  let edefs, pdefs = defs_of_pred Sy.SMap.empty Sy.SMap.empty body_pred in
  let kvar_to_simple_Kvar (subs, sym) = C.Kvar (subs_apply_defs edefs pdefs subs |> simplify_subs, sym) in
  let senv = 
    Sy.SMap.mapi (fun bv (vv, sort, ks) -> 
		    List.map kvar_to_simple_Kvar ks |>	C.make_reft vv sort) pfree_env in
  let sgrd = pred_apply_defs edefs pdefs body_pred |> simplify_pred in
  let slhs = List.map kvar_to_simple_Kvar lhs_ks |> C.make_reft (C.vv_of_reft lhs) (C.sort_of_reft lhs) in
  let rhs = C.rhs_of_t t in
  let rhs_ps, rhs_ks = preds_kvars_of_reft rhs in
  let srhs_pred = pred_apply_defs edefs pdefs (Ast.pAnd rhs_ps) |> simplify_pred in
  let srhs_ks = List.map kvar_to_simple_Kvar rhs_ks in
  let srhs =  (if P.is_tauto srhs_pred then srhs_ks else (C.Conc srhs_pred) :: srhs_ks) |> 
      C.make_reft (C.vv_of_reft rhs) (C.sort_of_reft rhs) in
    C.make_t senv sgrd slhs srhs (Some (C.id_of_t t)) (C.tag_of_t t)

let is_tauto_t t =
  match C.rhs_of_t t |> C.ras_of_reft with
    | [] -> true
    | [C.Conc p] -> P.is_tauto p 
    | _ -> false
