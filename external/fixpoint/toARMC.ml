(* translation to ARMC*)

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

let symbol_to_string = Ast.Symbol.to_string

let mk_kv_scope out ts wfs =
  output_string out "% kv -> scope:\n";
  let kvs = List.map kvars_of_t ts |> List.flatten |> List.map snd
    |> List.map symbol_to_string |> Misc.sort_and_compact in
  let kv_scope =
    List.fold_left
      (fun m wf ->
	 match C.reft_of_wf wf with
	   | vv, sort, [C.Kvar([], kvar)] ->
	       let v = symbol_to_string kvar in
	       let scope = 
		 card_vname :: val_vname ::
		   (C.env_of_wf wf |> C.bindings_of_env |> 
			List.map fst |> List.map symbol_to_string |> 
			    List.sort compare) in
		 Printf.fprintf out "%% %s -> %s\n"
		   v (String.concat ", " scope);
		 StrMap.add v scope m
	   | _ ->  (* Andrey: TODO print ill-formed wf *)
	       failure "ERROR: kname_scope_map: ill-formed wf" ""
      ) StrMap.empty wfs in
    {kvs = kvs; kv_scope = kv_scope}

let mk_data_var ?(suffix = "") kv v = 
  Printf.sprintf "_%s_%s%s%s" kv v (if suffix = "" then "" else "_") suffix

let mk_vs ?(suffix = "") s = 
  List.map 
    (fun kv ->
       try StrMap.find kv s.kv_scope |> List.map (mk_data_var ~suffix:suffix kv)
       with Not_found -> failure "ERROR: rel_state_vs: scope not found" kv
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

let mk_rule from_pc from_vs to_pc to_vs guards updates id = 
  Printf.sprintf
    "
r(p(pc(%s), data(%s)), 
  p(pc(%s), data(%s)), 
  [%s], 
  [%s], 
  %s)." 
    from_pc (String.concat ", " from_vs) 
    to_pc (String.concat ", " to_vs) 
    (if guards = [] then "1=1" else String.concat ", " guards)
    (mk_update_str from_vs to_vs updates)
    id

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
	    state.kvs) "t_init")


(*
  make -f Makefile.fixtop && ./f -latex /tmp/main.tex -armc /tmp/a.pl tests/pldi08-max.fq && cat /tmp/a.pl
*)
