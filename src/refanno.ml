module Ic = Inferctypes

open Cil

type cloc = int

type refgen = Ctypes.sloc
type refinst = cloc * Ctypes.sloc

let fresh_cloc =
  let i = ref cbot in
  (fun () -> incr i; !i)

type annotated_block = ((refgen option) * (refinst option)) list

let ens_opt = function
  | Some x -> x
  | None -> failwith "None passed to ens_opt"

let rec loc_of_var_expr get_theta = function
  | Lval (Var v, _) ->
      Some (get_theta v)
  | BinOp (_, e1, e2, _) ->
      let l1 = loc_of_var_expr e1 in
      let l2 = loc_of_var_expr e2 in
      if l1 = l2 then l1 else None
  | CastE (_, e) -> loc_of_var_expr e
  | _ -> None

let sloc_of_e ctm e =
  match Ic.ExpMap.find e ctm with
  | CTInt _ -> None
  | CTRef (s, _) -> Some s

let instantiate conc s c =
  if Hashtbl.mem conc s then
    if Hashtbl.find conc s = c then
      (None, None)
    else
      let _ = Hashtbl.replace conc s c in
      (Some s, Some (s, c))
  else
    let _ = Hashtbl.replace conc s c in
    (None, Some (s, c)

let get_th th v =
  try Hashtbl.find th t with
    Not_found ->
      let c = fresh_cloc () in
      Hashtbl.replace th c; c

let set_th = Hashtbl.replace

let annotate_set theta conc ctm = function
  | Cil.Set ((Var v1, _), Mem (Lval (Var v2, _) as e), _) ->
      instantiate (ens_opt (sloc_of_e ctm e) (get_theta v2)
  | Cil.Set ((Var v, _), e, _) ->
      let _ = match loc_of_var_expr e with
                | Some c -> set_theta v c
                | None -> () in
      (None, None)
  | Cil.Set ((Mem (Lval(Var v, _) as e), _), _, _) ->
      instantiate (ens_opt (sloc_of_e ctm e) (get_theta v)
  | _ -> (None, None)

let rec annotate_block theta conc ctm = function
  | instr :: instrs ->
      let anno = annotation_instr theta conc ctm instr ::
        annotate_block theta cons instrs
  | [] -> []

let annotate_block theta =
  (annotate_block theta (Hashtbl.create 17), theta)
