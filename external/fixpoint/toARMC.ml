(* translation to ARMC*)

module C  = FixConstraint
open Misc.Ops

let q name = Printf.sprintf "\'%s\'" name

let mk_data_var ?(suffix = "") vname = 
  Printf.sprintf "_%s%s%s" vname (if suffix = "" then "" else "_") suffix

let mk_data ?(suffix = "") vars = 
  Printf.sprintf "data(%s)" 
    (List.map (mk_data_var ~suffix:suffix) vars |> String.concat ", ")

let mk_data_primed vars = mk_data ~suffix:"p" vars

let kvars_of_t t = 
  List.fold_left
    (fun sofar reft -> C.kvars_of_reft reft ++ sofar)
    []
    (Ast.Symbol.SMap.fold
       (fun _ reft sofar -> reft :: sofar) 
       (C.env_of_t t) [C.lhs_of_t t; C.rhs_of_t t])


let to_armc out cs ws =
  print_endline "Translating to ARMC.";
  let kvars = 
    List.map kvars_of_t cs |> List.flatten |> List.map snd
      |> List.map Ast.Symbol.to_string |> Misc.sort_and_compact
  in 
  let from_data_str = mk_data kvars in
  let to_data_str = mk_data_primed kvars in
  let var2names = 
    List.map (fun v -> Printf.sprintf "(%s, %s)" (mk_data_var v) (q v)) kvars
    |> String.concat ", "
  in
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
      from_data_str to_data_str (* trans_preds *)
      from_data_str var2names (* var2names *)
