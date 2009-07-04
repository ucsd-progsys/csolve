(* translation to ARMC *)

module C  = FixConstraint
open Misc.Ops
module StrMap = Map.Make (struct type t = string let compare = compare end)


(* Andrey: TODO get rid of grd in t? grd p is a binding v:{v:b|p} *)
(* Andrey: TODO move to fixConstraint.ml? *)
let kvars_of_t t = 
  List.fold_left
    (fun sofar reft -> C.kvars_of_reft reft ++ sofar)
    []
    (Ast.Symbol.SMap.fold (fun _ reft sofar -> reft :: sofar) 
       (C.env_of_t t) [C.lhs_of_t t; C.rhs_of_t t])

let refa_to_string = function
  | C.Conc p -> Ast.Predicate.to_string p
  | C.Kvar (subs, sym) ->
      Printf.sprintf "%s%s" (Ast.Symbol.to_string sym)
	(List.map
	   (fun (s, e) -> 
	      Printf.sprintf "[%s/%s]" 
		(Ast.Expression.to_string e) (Ast.Symbol.to_string s)
	   ) subs |> String.concat "")
let reft_to_string reft =
  Printf.sprintf "{%s:%s | [%s]}"
    (C.vv_of_reft reft |> Ast.Symbol.to_string)
    (C.sort_of_reft reft |> Ast.Sort.to_string)
    (C.ras_of_reft reft |> List.map refa_to_string |> String.concat ", ")
let binding_to_string (vv, reft) =
  Printf.sprintf "%s:%s" (Ast.Symbol.to_string vv) (reft_to_string reft)

let start_pc = "start"
let loop_pc = "loop"
let error_pc = "error"
let val_vname = "VVVV"
let card_vname = "CARD"
let exists_kv = "EX"
let primed_suffix = "p"

type kv_scope = {
  kvs : string list;
  kv_scope : string list StrMap.t
}

let sanitize_symbol = Str.global_replace (Str.regexp "@") "_at_" 

let symbol_to_armc s = Ast.Symbol.to_string s |> sanitize_symbol

let mk_data_var ?(suffix = "") kv v = 
  Printf.sprintf "_%s_%s%s%s" 
    (sanitize_symbol kv) (sanitize_symbol v) (if suffix = "" then "" else "_") suffix

let constant_to_armc = Ast.Constant.to_string
let bop_to_armc = function 
  | Ast.Plus  -> "+"
  | Ast.Minus -> "-"
  | Ast.Times -> "*"
  | Ast.Div   -> "/"
let brel_to_armc = function 
  | Ast.Eq -> "="
  | Ast.Ne -> "=\\="
  | Ast.Gt -> ">= 1+"
  | Ast.Ge -> ">="
  | Ast.Lt -> "+1 =<"
  | Ast.Le -> "=<"
let bind_to_armc (s, t) = (* Andrey: TODO support binders *)
  Printf.sprintf "%s:%s" (symbol_to_armc s) (Ast.Sort.to_string t |> sanitize_symbol)
let rec expr_to_armc (e, _) = 
  match e with
    | Ast.Con c -> constant_to_armc c
    | Ast.Var s -> mk_data_var exists_kv (symbol_to_armc s)
    | Ast.App (s, es) ->
	let str = symbol_to_armc s in
	  if es = [] then str else
	    Printf.sprintf "%s(%s)" str (List.map expr_to_armc es |> String.concat ", ")
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
    | Ast.True -> "1=1"
    | Ast.False -> "0=1"
    | Ast.Bexp e -> expr_to_armc e
    | Ast.Not p -> (* Andrey: TODO support negation *)
	Printf.sprintf "neg(%s)" (pred_to_armc p) 
    | Ast.Imp (p1, p2) -> (* Andrey: TODO support implication *)
	Printf.sprintf "imp(%s, %s)" (pred_to_armc p1) (pred_to_armc p2)
    | Ast.And ps -> 
	if ps = [] then "1=1" else
	  List.map pred_to_armc ps |> String.concat ", "
    | Ast.Or ps -> (* Andrey: TODO support disjunction *) 
	begin
	  match ps with
	    | [] -> "0=1"
	    | [p] -> pred_to_armc p
	    | _::_ ->
		Printf.sprintf "(%s)" 
		  (List.map pred_to_armc ps |> String.concat "; ")
	end
    | Ast.Atom (e1, r, e2) ->
	Printf.sprintf "%s %s %s" 
          (expr_to_armc e1) (brel_to_armc r) (expr_to_armc e2)
    | Ast.Forall (qs,p) -> (* Andrey: TODO support forall *) 
	Printf.sprintf "forall([%s], %s)" 
          (List.map bind_to_armc qs |> String.concat ", ") 
	  (pred_to_armc p)


let mk_kv_scope out ts wfs =
  output_string out "% kv -> scope:\n";
  let kvs = List.map kvars_of_t ts |> List.flatten |> List.map snd
    |> List.map symbol_to_armc |> Misc.sort_and_compact in
  let kv_scope =
    List.fold_left
      (fun m wf ->
	 match C.reft_of_wf wf with
	   | vv, sort, [C.Kvar([], kvar)] ->
	       let v = symbol_to_armc kvar in
	       let scope = 
		 card_vname :: val_vname ::
		   (C.env_of_wf wf |> C.bindings_of_env |> 
			List.map fst |> List.map symbol_to_armc |> 
			    List.sort compare) in
		 Printf.fprintf out "%% %s -> %s\n"
		   v (String.concat ", " scope);
		 StrMap.add v scope m
	   | _ ->  (* Andrey: TODO print ill-formed wf *)
	       failure "ERROR: kname_scope_map: ill-formed wf"
      ) StrMap.empty wfs in
    {kvs = kvs; kv_scope = kv_scope}

let mk_vs ?(suffix = "") s = 
  List.map 
    (fun kv ->
       try StrMap.find kv s.kv_scope |> List.map (mk_data_var ~suffix:suffix kv)
       with Not_found -> failure "ERROR: rel_state_vs: scope not found for %s" kv
    ) s.kvs |> List.flatten

let mk_var2names state = 
  List.map
    (fun kv ->
       List.map 
	 (fun v -> 
	    Printf.sprintf "(%s, \'%s_%s\')"
	      (mk_data_var kv v)  kv v
	 ) (StrMap.find kv state.kv_scope) |> String.concat ", "
    ) state.kvs |> String.concat ", "

let mk_skip_update state kvs = 
  List.map
    (fun kv ->
       List.map 
	 (fun v -> 
	    Printf.sprintf "%s = %s"
	      (mk_data_var ~suffix:primed_suffix kv v) (mk_data_var kv v)
	 ) (StrMap.find kv state.kv_scope) |> String.concat ", "
    ) kvs |> String.concat ", "

let mk_update_str from_vs to_vs updates = 
  List.map2
    (fun v vp ->
       Printf.sprintf "%s = %s" vp (try StrMap.find v updates with Not_found -> v)
    ) from_vs to_vs |> String.concat ", "

let split_scope scope = 
  match scope with
    | card :: value :: data -> card, value, data
    | [] -> failure "ERROR: split_scope: empty scope %s" (String.concat ", " scope)

let reft_to_armc ?(suffix = "") state reft = 
  let vv = C.vv_of_reft reft |> symbol_to_armc in
  let rs = C.ras_of_reft reft in
    if rs = [] then "1=1" else
      List.map
	(function
	   | C.Conc pred -> pred_to_armc pred
	   | C.Kvar (subs, sym) -> 
	       let subs_map = List.fold_left
		 (fun m (s, e) -> StrMap.add (symbol_to_armc s) e m) StrMap.empty subs in
	       let find_subst v default = 
		 try StrMap.find v subs_map |> expr_to_armc with Not_found -> default in
	       let kv = symbol_to_armc sym in
	       let card, value, data = StrMap.find kv state.kv_scope |> split_scope in
		 Printf.sprintf "%s = 1" (mk_data_var ~suffix:suffix  kv card) 
		 :: Printf.sprintf "%s = %s" 
		   (mk_data_var ~suffix:suffix kv value) 
		   (find_subst vv (mk_data_var exists_kv vv)) 
		 :: List.map
		   (fun v -> 
		      Printf.sprintf "%s = %s"
			(mk_data_var ~suffix:suffix kv v)
			(find_subst v (mk_data_var exists_kv v))
		   ) data |> String.concat ", "
	) rs |> String.concat ", "

let mk_rule from_pc from_data to_pc to_data annot_guards annot_updates id = 
  let rec annot_conj_to_armc = function
    | (g, a) :: rest -> 
	if rest = [] then Printf.sprintf "\n   %s \t%% %s\n  ]," g a
	else Printf.sprintf "\n   %s, \t%% %s%s" g a (annot_conj_to_armc rest)
    | [] -> "],"
  in
    Printf.sprintf
      "
r(p(pc(%s), data(%s)), 
  p(pc(%s), data(%s)),
  [%s
  [%s
  %s).
" 
      from_pc from_data to_pc to_data
      (annot_conj_to_armc annot_guards)
      (annot_conj_to_armc annot_updates)
      id

let t_to_armc from_data to_data state t = 
  let grd = C.grd_of_t t in
  let lhs = C.lhs_of_t t in
  let rhs = C.rhs_of_t t in
  let tag = try string_of_int (C.id_of_t t) with _ -> 
    failure "ERROR: t_to_armc: anonymous constraint %s" (C.to_string t) in
    match rhs with 
      | (_, _, [C.Kvar (_, sym)]) ->
	  let kv = symbol_to_armc sym in
	    mk_rule "loop" from_data "loop" to_data
	      (List.map
		 (fun (bv, reft) ->
		    reft_to_armc state (C.theta [(C.vv_of_reft reft, Ast.eVar bv)] reft),
		    binding_to_string (bv, reft)
		 ) (C.env_of_t t |> C.bindings_of_env) 
	       ++
		 [(pred_to_armc grd, Ast.Predicate.to_string grd); 
		  (reft_to_armc state lhs, "|- " ^ (reft_to_string lhs))])
	      [(reft_to_armc ~suffix:primed_suffix state rhs, "<: " ^ (reft_to_string rhs));
	       (List.filter (fun kv' -> kv <> kv') state.kvs |> mk_skip_update state, "skip")]
	      tag
      | (_, _, rs) -> "Andrey: TODO t_to_armc "

let to_armc out ts wfs =
  print_endline "Translating to ARMC.";
  let state = mk_kv_scope out ts wfs in
  let from_data = mk_vs state |> String.concat ", " in
  let to_data = mk_vs ~suffix:primed_suffix state |> String.concat ", " in
    Printf.fprintf out
      ":- multifile r/5,implicit_updates/0,var2names/2,preds/2,trans_preds/3,cube_size/1,start/1,error/1,refinement/1,cutpoint/1,invgen_template/2,invgen_template/1,cfg_exit_relation/1,stmtsrc/2,strengthening/2.

refinement(inter).
cube_size(1).

start(pc(%s)).
error(pc(%s)).
cutpoint(pc(%s)).

preds(p(_, data(%s)), []).

trans_preds(p(_, data(%s)), p(_, data(%s)), []).

var2names(p(_, data(%s)), [%s]).
"
      start_pc loop_pc error_pc
      from_data (* preds *)
      from_data to_data (* trans_preds *)
      from_data (mk_var2names state); (* var2names *)
    output_string out 
      (mk_rule start_pc from_data loop_pc to_data [] 
	 [(List.map 
	     (fun kv -> 
		let card, _, _ = StrMap.find kv state.kv_scope |> split_scope in
		  Printf.sprintf "%s = 0" (mk_data_var ~suffix:primed_suffix kv card)
	     ) state.kvs |> String.concat ", ", 
	   "")]
         "t_init");
    List.iter (fun t -> t_to_armc from_data to_data state t |> output_string out) ts


(*
  make -f Makefile.fixtop && ./f -latex /tmp/main.tex -armc /tmp/a.pl tests/pldi08-max.fq && cat /tmp/a.pl
*)
