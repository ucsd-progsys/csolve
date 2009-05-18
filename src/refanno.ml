module Ic = Inferctypes

open Cil

type cloc = int

let cbot = 0

type annotation =
  RefGen of Ctypes.sloc | RefInst of cloc * Ctypes.sloc

let fresh_cloc =
  let i = ref cbot in
  (fun () -> incr i; !i)

type annotated_block = (annotation option * annotation option) list

(* two cases: v/op(v, _) or *v *)
let locs_of_expr e =
  let rec locs_rec = function
    | Cil.Lval(lv) ->
        (match lv with
          | Mem (Lval (Var v)) -> (* find 
          | Var v -> theta v
          | _ -> failwith "Unexpected lval in locs_of_expr" )
    | Cil.CastE(e, _)
    | Cil.SizeOfE(e)
    | Cil.UnOp(_, e, _) ->
    | Cil.AddrOf(
    | Cil.StartOf(
    | Cil.BinOp (e1, e2, _) -> in
  match locs_rec e


let rec annotate_block theta conc ctm = function
  | Cil.Set ((Var v, off), e, _) :: instrs ->  
      (match Ic.ExpMap.find e ctm with 
        | CTInt _ -> (None, None) :: annotate_block theta conc instrs
        | CTRef (s', _) -> 
            let 
            let theta = (fun s' ->  
            (None, None) :: annotate_block ( ))
  | Cil.Set ((Mem em, off), e, _) :: instrs ->
      (match Ic.ExpMap.find e ctm with
        | CTInt _ -> (None, 
        | CTRef (s, _) ->  )
  | Cil.Set ((Var v, off), Const _, _) :: instrs 
  | instr :: instrs -> (None, None) :: annotate_block theta conc instrs
  | [] -> []

let annotate_block =
  annotate_block (fun (x: Cil.varinfo) -> bot)
    (fun (x: Ctypes.sloc) -> bot)

val annotate_block: Cil.instr list -> (annotated_block * (Cil.varinfo -> cloc))
