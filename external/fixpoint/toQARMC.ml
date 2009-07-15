(* translation to Q'ARMC *)

module C  = FixConstraint
module Co = Constants 
module StrMap = Map.Make (struct type t = string let compare = compare end)
module StrSet = Set.Make (struct type t = string let compare = compare end)
open Misc.Ops

(* Andrey: TODO move to ast.ml? *)
let pred_is_atomic (p, _) =
  match p with
    | Ast.True | Ast.False | Ast.Bexp _ | Ast.Atom _ -> true
    | Ast.And _ | Ast.Or _ | Ast.Not _ | Ast.Imp _ | Ast.Forall _ -> false

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
  sol : Ast.pred list Ast.Symbol.SMap.t;
}

let sanitize_symbol s = 
  Str.global_replace (Str.regexp "@") "_at_"  s |> Str.global_replace (Str.regexp "#") "_hash_" |>
      Str.global_replace (Str.regexp "\.") "_dot_" |> Str.global_replace (Str.regexp "'") "_q_" 

let symbol_to_armc s = Ast.Symbol.to_string s |> sanitize_symbol

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
    | Ast.True -> "1=1"
    | Ast.False -> "0=1"
    | Ast.Bexp e -> Printf.sprintf "%s = 1" (expr_to_armc e)
    | Ast.Not p -> Printf.sprintf "neg(%s)" (pred_to_armc p) 
    | Ast.Imp (p1, p2) -> Printf.sprintf "(neg(%s); %s)" (pred_to_armc p1) (pred_to_armc p2)
    | Ast.And [] -> "1=1"
    | Ast.And [p] -> pred_to_armc p
    | Ast.And (_::_ as ps) -> Printf.sprintf "(%s)" (List.map pred_to_armc ps |> String.concat ", ")
    | Ast.Or [] -> "0=1"
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


let mk_kv_scope out ts wfs sol =
  output_string out "% kv -> scope:\n";
  let kvs = List.map C.kvars_of_t ts |> List.flatten |> List.map snd |> 
      List.map symbol_to_armc |> (* (fun s -> Printf.sprintf "k%s" (symbol_to_armc s)) |> *)
	  Misc.sort_and_compact in
  let kv_scope =
    List.fold_left
      (fun m wf ->
	   match C.reft_of_wf wf |> C.ras_of_reft with
	     | [C.Kvar([], kvar)] ->
		 let v = symbol_to_armc kvar in
		 let scope = 
		   val_vname ::
		     (C.env_of_wf wf |> C.bindings_of_env |> List.map fst |> List.map symbol_to_armc |> 
			  List.filter (fun s -> not (Misc.is_prefix str__cil_tmp s)) |> List.sort compare) in
		   Printf.fprintf out "%% %s -> %s\n"
		     v (String.concat ", " scope);
		   StrMap.add v scope m
	     | _ -> m
		 (* Andrey: TODO handle ill-formed wf *)
(*		 Format.printf "%a" (C.print_wf None) wf;
		 failure "ERROR: kname_scope_map: ill-formed wf"
*)
      ) StrMap.empty wfs in
    {kvs = kvs; kv_scope = kv_scope; sol = sol}

let mk_data ?(suffix = "") ?(skip_kvs = []) s = 
  Printf.sprintf "[%s]"
    (List.map 
       (fun kv ->
	  try 
	    StrMap.find kv s.kv_scope |> 
		List.map (mk_data_var ~suffix:(if List.mem kv skip_kvs then "" else suffix) kv)
	  with Not_found -> failure "ERROR: rel_state_vs: scope not found for %s" kv
       ) s.kvs |> List.flatten |> String.concat ", ")

let mk_query ?(suffix = "") s kv = 
  Printf.sprintf "k%s(%s)" 
    kv (List.map (mk_data_var ~suffix:suffix kv) (StrMap.find kv s.kv_scope) |> String.concat ", ")

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

let mk_skip_update state kvs = 
  if kvs = [] then "1=1" else
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
    | value :: data -> value, data
    | _ -> failure "ERROR: split_scope: empty scope %s" (String.concat ", " scope)

let reft_to_armc ?(noquery = false) ?(suffix = "") state reft = 
  let vv = C.vv_of_reft reft |> symbol_to_armc in
  let rs = C.ras_of_reft reft in
    if rs = [] then "1=1" else
      List.map
	(function
	   | C.Conc pred -> pred_to_armc pred
	   | C.Kvar (subs, sym) -> 
	       if Ast.Symbol.SMap.mem sym state.sol && Ast.Symbol.SMap.find sym state.sol = [] then 
		 		 (Printf.printf "True %s\n" (Ast.Symbol.to_string sym);
		 "1=1") else
		 let subs_map = List.fold_left
		   (fun m (s, e) -> StrMap.add (symbol_to_armc s) e m) StrMap.empty subs in
		 let find_subst v default = 
		   try StrMap.find v subs_map |> expr_to_armc with Not_found -> default in
		 let kv = symbol_to_armc sym in
		 let value, data = StrMap.find kv state.kv_scope |> split_scope in
		   Printf.sprintf "%s%s = %s" 
		     (if noquery then "" else (mk_query ~suffix:suffix state kv) ^ ", ")
		     (mk_data_var ~suffix:suffix kv value) 
		     (find_subst vv (mk_data_var exists_kv vv)) 
		   :: List.map
		     (fun v -> 
			Printf.sprintf "%s = %s"
			  (mk_data_var ~suffix:suffix kv v)
			  (find_subst v (mk_data_var exists_kv v))
		     ) data |> String.concat ", "
	) rs |> String.concat ", "

let mk_rule head annot_guards annot_updates id = 
  let rec annot_conj_to_armc = function
    | (g, a) :: rest -> 
	if rest = [] then Printf.sprintf "\n   %s \t%% %s\n  ]," g a
	else Printf.sprintf "\n   %s, \t%% %s%s" g a (annot_conj_to_armc rest)
    | [] -> "],"
  in
    Printf.sprintf
      "
hc(%s, [%s  %s).
" 
      head (annot_guards @ annot_updates |> annot_conj_to_armc) id

let t_to_armc state t = 
  let grd = C.grd_of_t t in
  let lhs = C.lhs_of_t t in
  let rhs = C.rhs_of_t t in
  let rhs_s = C.reft_to_string rhs in
  let tag = try string_of_int (C.id_of_t t) with _ -> 
    failure "ERROR: t_to_armc: anonymous constraint %s" (C.to_string t) in
  let annot_guards = 
    Misc.map_partial
      (fun (bv, reft) ->
	 if C.ras_of_reft reft <> [] then
	   Some (reft_to_armc state (C.theta [(C.vv_of_reft reft, Ast.eVar bv)] reft),
		 C.binding_to_string (bv, reft))
	 else
	   None
      ) (C.env_of_t t |> C.bindings_of_env)
    ++ [(pred_to_armc grd, Ast.Predicate.to_string grd); 
	(reft_to_armc state lhs, "|- " ^ (C.reft_to_string lhs))] in
  let ps, kvs =  
    List.fold_left (fun (ps', kvs') refa ->
		      match refa with
			| C.Conc p -> p::ps', kvs'
			| C.Kvar (subs, sym) -> ps', (subs, sym)::kvs'
		   ) ([], []) (C.ras_of_reft rhs) in
    (if ps <> [] then
       [mk_rule error_pc annot_guards [(Ast.pAnd ps |> Ast.pNot |> pred_to_armc, "<: " ^ rhs_s)] tag]
     else 
       [])
    ++
      (List.map 
	 (fun (_, sym) ->
	    mk_rule (mk_query ~suffix:primed_suffix state (symbol_to_armc sym))
	      annot_guards 
	      [(reft_to_armc ~noquery:true ~suffix:primed_suffix state rhs, "<: " ^ rhs_s)]
	      tag
	 ) kvs)


let to_qarmc out ts wfs sol =
  print_endline "Translating to QARMC.";
  let state = mk_kv_scope out ts wfs sol in
    Printf.fprintf out
      ":- multifile hc/3, var2names/2, preds/2, error/1.

error(%s).
%s
"
      error_pc
      (mk_var2names state);
    List.iter (fun t -> t_to_armc state t |> List.iter (output_string out)) ts


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
