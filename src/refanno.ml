module Ct = Ctypes

open Cil

type cloc = string 
type ctab = (string, cloc) Hashtbl.t

type refgen = Ctypes.sloc
type refinst = Ctypes.sloc * cloc
type annotation = (refgen option) * (refinst option)
type block_annotation = annotation list

let fresh_cloc =
  let i = ref 0 in
  (fun () -> incr i; "s" ^ (string_of_int !i))

let ens_opt = function
  | Some x -> x
  | None -> failwith "None passed to ens_opt"

let cloc_of_v th v =
  try Hashtbl.find th v.vname with
    Not_found ->
      let c = fresh_cloc () in
      Hashtbl.replace th v.vname c; c

let set_cloc th v c = Hashtbl.replace th v.vname c

let loc_of_var_expr theta =
  let rec loc_rec = function
    | Lval (Var v, _) ->
        Some (cloc_of_v theta v)
    | BinOp (_, e1, e2, _) ->
        let l1 = loc_rec e1 in
        let l2 = loc_rec e2 in
        if l1 = l2 then l1 else None
    | CastE (_, e) -> loc_rec e
    | _ -> None in
  loc_rec

let sloc_of_e ctm e =
  match Inferctypes.ExpMap.find e ctm with
  | Ct.CTInt _ -> None
  | Ct.CTRef (s, _) -> Some s

let instantiate conc s c =
  if Hashtbl.mem conc s then
    if Hashtbl.find conc s = c then
      (None, None)
    else
      let _ = Hashtbl.replace conc s c in
      (Some s, Some (s, c))
  else
    let _ = Hashtbl.replace conc s c in
    (None, Some (s, c))

let annotate_instr theta conc ctm = function
    (* v1 := *v2 *)
  | Cil.Set ((Var v1, _), Lval (Mem (Lval (Var v2, _) as e), _), _) ->
      instantiate conc (ens_opt (sloc_of_e ctm e)) (cloc_of_v theta v2)
    (* v := e *)
  | Cil.Set ((Var v, _), e, _) ->
      let _ = match loc_of_var_expr theta e with
                | Some c -> set_cloc theta v c
                | None -> () in
      (None, None)
    (* *v := e *)
  | Cil.Set ((Mem (Lval(Var v, _) as e), _), _, _) ->
      instantiate conc (ens_opt (sloc_of_e ctm e)) (cloc_of_v theta v)
  | _ -> (None, None)

let rec annotate_block theta conc ctm = function
  | instr :: instrs ->
      annotate_instr theta conc ctm instr
      :: annotate_block theta conc ctm instrs
  | [] -> []
 
let annotate_block (theta: ctab) (conc: (Ctypes.sloc, cloc) Hashtbl.t) (ctm: Inferctypes.ctemap)
                   (instrs: Cil.instr list) : block_annotation =
  annotate_block theta conc ctm instrs

  (* API *)
let annotate_cfg (c: Ssa.cfgInfo) (ctm: Inferctypes.ctemap): (block_annotation array * ctab) =
  let (sa, i) = (Array.make (Array.length c.Ssa.blocks) [], ref 0) in
  let (theta, conc) = (Hashtbl.create 17, Hashtbl.create 17) in
  Array.iter
    (fun b ->
      match b.Ssa.bstmt.skind with
        | Instr instrs ->
            Array.set sa !i (annotate_block theta conc ctm instrs);
            Hashtbl.clear conc; incr i
        | _ -> incr i) c.Ssa.blocks;
  (sa, theta)

  (* API *)
let cloc_of_v_pub (c: ctab) (v: Cil.varinfo) : cloc = Hashtbl.find c v.vname
