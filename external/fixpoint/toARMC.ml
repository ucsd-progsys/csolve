(* translation to ARMC *)

module C  = FixConstraint
open Misc.Ops
module StrMap = Map.Make (struct type t = string let compare = compare end)

type kv_scope = {
  kvs : string list;
  kv_scope : string list StrMap.t
}

(* Andrey: TODO move to fixConstraint.ml? *)
let kvars_of_t t = 
  List.fold_left
    (fun sofar reft -> C.kvars_of_reft reft ++ sofar)
    []
    (Ast.Symbol.SMap.fold (fun _ reft sofar -> reft :: sofar) 
       (C.env_of_t t) [C.lhs_of_t t; C.rhs_of_t t])

let val_vname = "VVVV"
let card_vname = "CARD"

let q s = Printf.sprintf "\'%s\'" s

let HEHEHEE

let mk_data_var ?(suffix = "") kv v = 
  Printf.sprintf "_%s_%s%s%s" kv v (if suffix = "" then "" else "_") suffix

let symbol_to_armc ?(kv = "") s = 
  Ast.Symbol.to_string s |> Str.global_replace (Str.regexp "@") "_at_" 

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
let bind_to_armc (s, t) = 
  failure "ERROR: bind_to_armc %s:%s" (symbol_to_armc s) (Ast.Sort.to_string t)
let rec expr_to_armc ?(kv = "EX") (e, _) = 
  match e with
    | Ast.Con c -> constant_to_armc c
    | Ast.Var s -> symbol_to_armc ~kv:kv s
    | Ast.App (s, es) ->
	let str = symbol_to_armc ~kv:kv s in
	  if es = [] then str else
	    Printf.sprintf "%s(%s)" str
	      (List.map (expr_to_armc ~kv:kv) es |> String.concat " ")
    | Ast.Bin (e1, op, e2) ->
	Printf.sprintf "(%s %s %s)" 
	  (expr_to_armc ~kv:kv e1) (bop_to_armc op) (expr_to_armc ~kv:kv e2)
    | Ast.Ite (ip, te, ee) -> 
	Printf.sprintf "ite(%s, %s, %s)" 
	  (pred_to_armc ~kv:kv ip) (expr_to_armc ~kv:kv te) (expr_to_armc ~kv:kv ee)
    | Ast.Fld (s, e) -> 
	Printf.sprintf "fld(%s, %s)" (expr_to_armc ~kv:kv e) (symbol_to_armc ~kv:kv s)
and pred_to_armc ?(kv = "EX") (p, _) = 
  match p with
    | Ast.True -> "1=1"
    | Ast.False -> "0=1"
    | Ast.Bexp e -> expr_to_armc ~kv:kv e
    | Ast.Not p -> (* Andrey: TODO support negation *)
	Printf.sprintf "neg(%s)" (pred_to_armc ~kv:kv p) 
    | Ast.Imp (p1, p2) -> (* Andrey: TODO support implication *)
	Printf.sprintf "imp(%s, %s)" 
	  (pred_to_armc ~kv:kv p1) (pred_to_armc ~kv:kv p2)
    | Ast.And ps -> 
	if ps = [] then "1=1" else
	  List.map (pred_to_armc ~kv:kv) ps |> String.concat ", "
    | Ast.Or ps -> (* Andrey: TODO support disjunction *) 
	begin
	  match ps with
	    | [] -> "0=1"
	    | [p] -> pred_to_armc ~kv:kv p
	    | _::_ ->
		Printf.sprintf "(%s)" 
		  (List.map (pred_to_armc ~kv:kv) ps |> String.concat "; ")
	end
    | Ast.Atom (e1, r, e2) ->
	Printf.sprintf "(%s %s %s)" 
          (expr_to_armc ~kv:kv e1) (brel_to_armc r) (expr_to_armc ~kv:kv e2)
    | Ast.Forall (qs,p) -> (* Andrey: TODO support forall *) 
	Printf.sprintf "forall([%s], %s)" 
          (List.map bind_to_armc qs |> String.concat ", ") 
	  (pred_to_armc ~kv:kv p)


let mk_kv_scope out ts wfs =
  output_string out "% kv -> scope:\n";
  let kvs = List.map kvars_of_t ts |> List.flatten |> List.map snd
    |> List.map symbol_to_armc |> Misc.sort_and_compact in
  let kv_scope =
    List.fold_left
      (fun m wf ->
	 match C.reft_of_wf wf with
	   | vv, sort, [C.Kvar([], kvar)] ->
	       let v = mk_data_var kvar in
	       let scope = 
		 card_vname :: val_vname ::
		   (C.env_of_wf wf |> C.bindings_of_env |> 
			List.map fst |> List.map mk_data_var |> 
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

let mk_var2names vs = 
  Printf.sprintf "%s" 
    (List.map
       (fun v -> 
	  Printf.sprintf "(%s, %s)"
	    v
	    (String.sub v 1 ((String.length v)-1) |> q)) vs |>
	   String.concat ", ")

let mk_update_str from_vs to_vs updates = 
  List.map2
    (fun v vp ->
       Printf.sprintf "%s = %s" vp (try StrMap.find v updates with Not_found -> v)
    ) from_vs to_vs |> String.concat ", "

(*
type tag  = int
type subs = (Ast.Symbol.t * Ast.expr) list            (* [x := e] *) 
type refa = Conc of Ast.pred | Kvar of subs * Ast.Symbol.t
type reft = Ast.Symbol.t * Ast.Sort.t * (refa list)   (* { VV: t | [ra] } *)
*)

let refa_to_armc = function
  | C.Conc pred -> Ast.Predicate.to_string pred
  | C.Kvar (subs, sym) -> 
      Printf.sprintf "%s%s" 
	(Ast.Symbol.to_string sym)
	(List.map (fun (s, e) -> Printf.sprintf "[%s/%s]"
		     (Ast.Expression.to_string e)
		     (Ast.Symbol.to_string s)
		  ) subs |> String.concat "")

let mk_rule from_pc from_vs to_pc to_vs guards updates id = 
  Printf.sprintf
    "
r(p(pc(%s), data(%s)), 
  p(pc(%s), data(%s)), 
  [%s], 
  [%s], 
  %s).
" 
    from_pc (String.concat ", " from_vs) 
    to_pc (String.concat ", " to_vs) 
    (if guards = [] then "1=1" else String.concat ", " guards)
    (mk_update_str from_vs to_vs updates)
    id

let t_to_armc from_vs to_vs t = 
  mk_rule "loop" from_vs "loop" to_vs
(*
    (C.env_of_t c |> envt_to_armc)  
*)
    [C.grd_of_t t |> pred_to_armc]
(*
    (C.lhs_of_t c |> reft_to_armc) 
    (C.rhs_of_t c |> reft_to_armc)
*)
     StrMap.empty
    "dummy_id"
(* Andrey: TODO
    (try string_of_int (C.id_of_t t) with _ -> 
       failure "ERROR: t_to_armc: anonymous constraint %s" (C.to_string t))
*)

let to_armc out ts wfs =
  print_endline "Translating to ARMC.";
  let state = mk_kv_scope out ts wfs in
  let from_vs = mk_vs state in
  let to_vs = mk_vs ~suffix:"p" state in
  let from_data_str = String.concat ", " from_vs in 
    Printf.fprintf out
      ":- multifile r/5,implicit_updates/0,var2names/2,preds/2,trans_preds/3,cube_size/1,start/1,error/1,refinement/1,cutpoint/1,invgen_template/2,invgen_template/1,cfg_exit_relation/1,stmtsrc/2,strengthening/2.

refinement(inter).
cube_size(1).

start(pc(start)).
error(pc(error)).
cutpoint(pc(loop)).

preds(p(_, data(%s)), []).

trans_preds(p(_, data(%s)), p(_, data(%s)), []).

var2names(p(_, data(%s)), [%s]).
"
      from_data_str (* preds *)
      from_data_str (String.concat ", " to_vs) (* trans_preds *)
      from_data_str (mk_var2names from_vs); (* var2names *)
    output_string out 
      (mk_rule "start" from_vs "loop" to_vs [] 
	 (List.fold_left
	    (fun m kv ->
	       StrMap.add 
		 (mk_data_var kv (StrMap.find kv state.kv_scope |> List.hd)) "0" m
	    )
	    StrMap.empty
	    state.kvs) "t_init");
    List.iter (fun t -> t_to_armc from_vs to_vs t |> output_string out) ts


(*
  make -f Makefile.fixtop && ./f -latex /tmp/main.tex -armc /tmp/a.pl tests/pldi08-max.fq && cat /tmp/a.pl
*)
