(* translation to ARMC*)

module C  = FixConstraint
open Misc.Ops

let mk_data_var ?(suffix = "") vname = 
  Printf.sprintf "_%s%s%s" vname (if suffix = "" then "" else "_") suffix

let mk_data ?(suffix = "") vars = 
  Printf.sprintf "data(%s)" 
    (List.map (mk_data_var ~suffix:suffix) vars |> String.concat ", ")

let mk_data_primed vars = mk_data ~suffix:"p" vars


let to_armc out cs ws =
  print_endline "Translating to ARMC.";
  let kvars = 
    List.map C.kvars_of_t cs |> List.flatten |> List.map snd
      |> List.map Ast.Symbol.to_string |> List.sort compare
  in 
    Printf.printf "data vars: %s\n" (mk_data kvars);
    Printf.printf "data vars primed: %s\n" (mk_data_primed kvars);
