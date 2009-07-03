(* translation to ARMC*)

module C  = FixConstraint
open Misc.Ops
module StrMap = Map.Make (struct type t = string let compare = compare end)

let val_vname = "VVVV"
let card_vname = "CARD"

let q s = Printf.sprintf "\'%s\'" s

let symbol_to_string = Ast.Symbol.to_string

let mk_data_var prefix ?(suffix = "") v = 
  Printf.sprintf "%s_%s%s%s" 
    prefix v (if suffix = "" then "" else "_") suffix

let mk_data ?(suffix = "") vs v_scope = 
  Printf.sprintf "data(%s)" 
    (List.map 
       (fun v ->
	  card_vname :: val_vname :: StrMap.find v v_scope |>
	      List.map (mk_data_var v ~suffix:suffix) |> 
		  String.concat ", "
       ) vs |> String.concat ", ")

let mk_data_primed = mk_data ~suffix:"p" 

let mk_var2names vs v_scope = 
  Printf.sprintf "%s" 
    (List.map 
       (fun v ->
	  card_vname :: val_vname :: StrMap.find v v_scope |>
	      List.map
	      (fun vs ->
		 Printf.sprintf "(%s, \'%s_%s\')" 
		   (mk_data_var v vs)
		   v vs
	      ) |> String.concat ", "
       ) vs |> String.concat ", ")

let kvars_of_t t = 
  List.fold_left
    (fun sofar reft -> C.kvars_of_reft reft ++ sofar)
    []
    (Ast.Symbol.SMap.fold (fun _ reft sofar -> reft :: sofar) 
       (C.env_of_t t) [C.lhs_of_t t; C.rhs_of_t t])


let to_armc out ts wfs =
  print_endline "Translating to ARMC.";
  let kvars = List.map kvars_of_t ts |> List.flatten
    |> List.map snd |> Misc.sort_and_compact in
  let vs = List.map symbol_to_string kvars in
  let v_scope = List.fold_left
    (fun m wf ->
       match C.reft_of_wf wf with
	 | vv, sort, [C.Kvar([], kvar)] ->
	     let v = symbol_to_string kvar in
	     let scope = C.env_of_wf wf |> C.bindings_of_env |> 
		 List.map fst |> List.map symbol_to_string |> 
		     List.sort compare in
	       Printf.printf "%s -> %s\n"
		 v 
		 (String.concat ", " scope);
	       StrMap.add v scope m
	 | _ ->  (* Andrey: TODO print wf *)
	     failure "ERROR: kname_scope_map: ill-formed wf" ""
    ) StrMap.empty wfs
  in
  let from_data_str = mk_data vs v_scope in
    Printf.fprintf out
      ":- multifile r/5,implicit_updates/0,var2names/2,preds/2,trans_preds/3,cube_size/1,start/1,error/1,refinement/1,cutpoint/1,invgen_template/2,invgen_template/1,cfg_exit_relation/1,stmtsrc/2,strengthening/2.

refinement(inter).
cube_size(1).

start(pc(start)).
error(pc(error)).
cutpoint(pc(loop)).

preds(p(_, %s), []).

trans_preds(p(_, %s), p(_, %s), []).

var2names(p(_, %s), [%s]).
"
      from_data_str (* preds *)
      from_data_str (mk_data_primed vs v_scope) (* trans_preds *)
      from_data_str (mk_var2names vs v_scope); (* var2names *)
    ()


(*
make -f Makefile.fixtop && ./f -latex /tmp/main.tex -armc /tmp/a.pl tests/pldi08-max.fq && cat /tmp/a.pl
*)
