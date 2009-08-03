(* translation to Q'ARMC *)


module C  = FixConstraint
module Co = Constants 
module Sy = Ast.Symbol
module P = Ast.Predicate
module E = Ast.Expression
module StrMap = Map.Make (struct type t = string let compare = compare end)
module StrSet = Set.Make (struct type t = string let compare = compare end)
open Misc.Ops

let strlist_to_strset = List.fold_left (fun s x -> StrSet.add x s) StrSet.empty

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
let rec simplify_tauto ((p, _) as pred) =
  match p with
    | Ast.Not p -> Ast.pNot (simplify_tauto p)
    | Ast.Imp (p, q) -> Ast.pImp (simplify_tauto p, simplify_tauto q) 
    | Ast.Forall (qs, p) -> Ast.pForall (qs, simplify_tauto p)
    | Ast.And ps -> 
	let ps' = List.map simplify_tauto ps |> List.filter (fun p -> not(P.is_tauto p)) in
	  if List.mem Ast.pFalse ps' then Ast.pFalse else
	    begin
	      match ps' with
		| [] -> Ast.pTrue
		| [p'] -> p'
		| _ :: _ -> Ast.pAnd ps'
	    end
    | Ast.Or ps -> 
	let ps' = List.map simplify_tauto ps in
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

let kvar_apply_defs edefs pdefs (subs, sym) = 
  List.map (fun (s, e) -> s, expr_apply_defs edefs pdefs e) subs, sym

let armc_true = "1=1"
let armc_false = "0=1"
(*
let armc_true = "true"
let armc_false = "false"
*)
let loop_pc = "loop"
let start_pc = "start"
let error_pc = "error"
let val_vname = "VVVV"
let exists_kv = "EX"
let primed_suffix = "p"
let str__cil_tmp = "__cil_tmp"

type kv_scope = {
  kvs : string list;
  kv_scope : string list StrMap.t;
  sol : Ast.pred list Sy.SMap.t;
}

type horn_clause = {
  body_pred : Ast.pred;
  body_kvars : (C.subs * Sy.t) list;
  head_pred : Ast.pred;
  head_kvars : (C.subs * Sy.t) list;
  tag : string;
}

let sanitize_symbol s = 
  Str.global_replace (Str.regexp "@") "_at_"  s |> Str.global_replace (Str.regexp "#") "_hash_" |>
      Str.global_replace (Str.regexp "\\.") "_dot_" |> Str.global_replace (Str.regexp "'") "_q_" 

let symbol_to_armc s = Sy.to_string s |> sanitize_symbol

let mk_data_var ?(suffix = "") kv v = 
  Printf.sprintf "_%s_%s%s%s" 
    (sanitize_symbol v) (sanitize_symbol kv) (if suffix = "" then "" else "_") suffix

let constant_to_armc = Ast.Constant.to_string
let bop_to_armc = function 
  | Ast.Plus  -> "+"
  | Ast.Minus -> "-"
  | Ast.Times -> "*"
  | Ast.Div   -> "/"
let brel_to_armc = function 
  | Ast.Eq -> "="
  | Ast.Ne -> "=\\="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.Lt -> "<"
  | Ast.Le -> "=<"
let bind_to_armc (s, t) = (* Andrey: TODO support binders *)
  Printf.sprintf "%s:%s" (symbol_to_armc s) (Ast.Sort.to_string t |> sanitize_symbol)
let rec expr_to_armc (e, _) = 
  match e with
    | Ast.Con c -> constant_to_armc c
    | Ast.Var s -> mk_data_var exists_kv (symbol_to_armc s)
    | Ast.App (s, es) -> 
	if !Co.purify_function_application then "_" else
	  let str = symbol_to_armc s in
	    if es = [] then str else
	      Printf.sprintf "f_%s(%s)" str (List.map expr_to_armc es |> String.concat ", ")
    | Ast.Bin (e1, op, e2) ->
	Printf.sprintf "(%s %s %s)" 
	  (expr_to_armc e1) (bop_to_armc op) (expr_to_armc e2)
    | Ast.Ite (ip, te, ee) -> 
	Printf.sprintf "ite(%s, %s, %s)" 
	  (pred_to_armc ip) (expr_to_armc te) (expr_to_armc ee)
    | Ast.Fld (s, e) -> 
	Printf.sprintf "fld(%s, %s)" (expr_to_armc e) (symbol_to_armc s)
and pred_to_armc (p, _) = 
    match p with
      | Ast.True -> armc_true
      | Ast.False -> armc_false
      | Ast.Bexp e -> Printf.sprintf "%s = 1" (expr_to_armc e)
      | Ast.Not (Ast.True, _) -> armc_false
      | Ast.Not (Ast.False, _) -> armc_true
      | Ast.Not p -> Printf.sprintf "neg(%s)" (pred_to_armc p) 
      | Ast.Imp (p1, p2) -> Printf.sprintf "imp(%s, %s)" (pred_to_armc p1) (pred_to_armc p2)
      | Ast.And [] -> armc_true
      | Ast.And [p] -> pred_to_armc p
      | Ast.And (_::_ as ps) -> 
	  Printf.sprintf "(%s)" (List.map pred_to_armc ps |> String.concat ", ")
      | Ast.Or [] -> armc_false
      | Ast.Or [p] -> pred_to_armc p
      | Ast.Or (_::_ as ps) -> Printf.sprintf "(%s)" (List.map pred_to_armc ps |> String.concat "; ")
      | Ast.Atom (e1, Ast.Eq, (Ast.Ite(ip, te, ee), _)) ->
	  let ip_str = pred_to_armc ip in
	  let e1_str = expr_to_armc e1 in
	    Printf.sprintf "((%s, %s = %s); (neg(%s), %s = %s))"
	      ip_str e1_str (expr_to_armc te) 
	      ip_str e1_str (expr_to_armc ee) 
      | Ast.Atom (e1, r, e2) ->
	  Printf.sprintf "%s %s %s" 
            (expr_to_armc e1) (brel_to_armc r) (expr_to_armc e2)
      | Ast.Forall (qs,p) -> (* Andrey: TODO support forall *) 
	  Printf.sprintf "forall([%s], %s)" 
            (List.map bind_to_armc qs |> String.concat ", ") 
	    (pred_to_armc p)

(* Andrey: TODO pull variables from guards into scope *)
let mk_kv_scope out ts wfs sol =
  let kv_scope_t =
    List.fold_left 
      (fun m (subs, sym) ->
	 let kv = symbol_to_armc sym in
	 let scope = 
	   List.filter (fun (v, (e, _)) -> 
			  match e with
			    | Ast.Var v' -> v <> v'
			    | _ -> true
		       ) subs |> 
	       List.map fst |> List.map symbol_to_armc |> strlist_to_strset in
	 let scope' = try StrMap.find kv m with Not_found -> StrSet.empty in
	   StrMap.add kv (StrSet.union scope scope') m
      ) StrMap.empty (List.map C.kvars_of_t ts |> List.flatten) in
  let kv_scope = 
    StrMap.map (fun scope -> val_vname :: (StrSet.elements scope |> List.sort compare)) kv_scope_t in
  let kvs = StrMap.fold (fun kv _ kvs -> kv :: kvs) kv_scope [] in
    StrMap.iter (fun kv scope ->
    		 Printf.fprintf out "%% %s -> %s\n" kv (String.concat ", " scope)) kv_scope;
    {kvs = kvs; kv_scope = kv_scope; sol = sol}

let mk_var2names state = 
  List.map
    (fun kv ->
       Printf.sprintf "var2names(p(pc(k%s), data(%s)), [%s])."
	 kv
	 (List.map (mk_data_var kv) (StrMap.find kv state.kv_scope) |> String.concat ", ")
	 (List.map 
	    (fun v -> 
	       Printf.sprintf "(%s, \'%s_%s\')" (mk_data_var kv v)  v kv
	    ) (StrMap.find kv state.kv_scope) |> String.concat ", ")
    ) state.kvs |> String.concat "\n"

let preds_kvars_of_reft reft =
  List.fold_left 
    (fun (ps, ks) r ->
       match r with
	 | C.Conc p -> p :: ps, ks
	 | C.Kvar (subs, kvar) -> ps, (subs, kvar) :: ks
    ) ([], []) (C.ras_of_reft reft)

let t_to_horn_clause t =
  let lhs_ps, lhs_ks = C.lhs_of_t t |> preds_kvars_of_reft in
  let body_ps, body_ks = 
    Sy.SMap.fold 
      (fun bv reft (ps, ks) -> 
	 let ps', ks' = preds_kvars_of_reft (C.theta [(C.vv_of_reft reft, Ast.eVar bv)] reft) in
	   List.rev_append ps' ps, List.rev_append ks' ks
      ) (C.env_of_t t) (C.grd_of_t t :: lhs_ps, lhs_ks) in
  let head_ps, head_ks = C.rhs_of_t t |> preds_kvars_of_reft in
    {
      body_pred = Ast.pAnd body_ps; 
      body_kvars = body_ks; 
      head_pred = Ast.pAnd head_ps; 
      head_kvars = head_ks;
      tag = try string_of_int (C.id_of_t t) with _ -> failure "ERROR: t_to_horn_clause: anonymous constraint %s" (C.to_string t)
    }

let simplify_horn_clause hc = 
  let body_edefs, body_pdefs = defs_of_pred Sy.SMap.empty Sy.SMap.empty hc.body_pred in
  let edefs, pdefs = defs_of_pred body_edefs body_pdefs hc.head_pred in
    (* Sy.SMap.iter (fun s e -> Printf.printf "%s -> %s\n" (Sy.to_string s) (E.to_string e)) edefs; *)
    {
      body_pred = pred_apply_defs edefs pdefs hc.body_pred |> simplify_tauto; 
      body_kvars = List.map (kvar_apply_defs edefs pdefs) hc.body_kvars; 
      head_pred = pred_apply_defs edefs pdefs hc.head_pred |> simplify_tauto;
      head_kvars = List.map (kvar_apply_defs edefs pdefs) hc.head_kvars; 
      tag = hc.tag
    }

let print_horn_clause hc = 
  Printf.printf "%s: %s, %s :- %s, %s\n"
    hc.tag 
    (P.to_string hc.head_pred)
    (List.map (fun (subs, kvar) -> C.refa_to_string (C.Kvar (subs, kvar))) hc.head_kvars |> String.concat ", ")
    (P.to_string hc.body_pred)
    (List.map (fun (subs, kvar) -> C.refa_to_string (C.Kvar (subs, kvar))) hc.body_kvars |> String.concat ", ")

let kvar_to_armcs ?(suffix = "") state (subs, sym) = 
  let subs_map = List.fold_left (fun m (s, e) -> StrMap.add (symbol_to_armc s) e m) StrMap.empty subs in
  let find_subst v default = try StrMap.find v subs_map |> expr_to_armc with Not_found -> default in
  let kv = symbol_to_armc sym in
  let scope = StrMap.find kv state.kv_scope in
    Printf.sprintf "k%s(%s)" 
      kv (List.map (mk_data_var ~suffix:suffix kv) scope |> String.concat ", ")
    :: List.map (fun v -> 
		   Printf.sprintf "%s = %s" 
		     (mk_data_var ~suffix:suffix kv v) (find_subst v (mk_data_var exists_kv v))
		) scope  

let hc_to_armc state hc =
  let mk_rule head body tag = Printf.sprintf "hc(%s, [%s], %s)." head body tag in
  let body = 
    pred_to_armc hc.body_pred :: (List.map (kvar_to_armcs state) hc.body_kvars |> List.flatten) |> 
	String.concat ", " in
  let prules = 
    if P.is_tauto hc.head_pred then []
    else [mk_rule error_pc  (Printf.sprintf "%s, %s" body (Ast.pNot hc.head_pred |> pred_to_armc)) hc.tag] in
  let single_kvar = List.length hc.head_kvars = 1 in
  let rules, _ =
    List.fold_left
      (fun (rules, n) kvar -> 
	 let head_armcs = kvar_to_armcs ~suffix:primed_suffix state kvar in
	   mk_rule 
	     (List.hd head_armcs)
	     (body :: List.tl head_armcs |> String.concat ", ")
	     (if single_kvar then hc.tag else Printf.sprintf "%s%d" hc.tag n)
	   :: rules,
	 n+1
      ) (prules, 1) hc.head_kvars in
    rules
    
let to_hc_armc out ts wfs sol =
  print_endline "Translating to HC'ARMC.";
  let state = mk_kv_scope out ts wfs sol in
    Printf.fprintf out
      ":- multifile hc/3, var2names/2, preds/2, error/1.

error(%s).
%s
"
      error_pc
      (mk_var2names state);
    List.iter (fun t -> 
		 let hc = t_to_horn_clause t |> simplify_horn_clause in 
		   List.iter (fun r -> 
				output_string out r;
				output_string out "\n\n"
			     ) (hc_to_armc state hc)
	      ) ts
(*
  make -f Makefile.fixtop && ./f -latex /tmp/main.tex -armc /tmp/a.pl tests/pldi08-max.fq && cat /tmp/a.pl

tests:

for file in `ls pldi08-*-atom.fq`; do ../f -latex /tmp/main.tex -armc /tmp/a.pl $file; head -n 1 /tmp/a.pl; armc a.pl | grep correct; done

pldi08-arraymax-atom.fq  pass
pldi08-max-atom.fq       pass
pldi08-foldn-atom.fq     pass
pldi08-sum-atom.fq       pass
mask-atom.fq             pass
samples-atom.fq          pass 

test00.c                 pass

*)
