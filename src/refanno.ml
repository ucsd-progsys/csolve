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

let rec annotate_block theta conc ctm = function
  | Cil.Set ((Var v, off), e, _) :: instrs ->  
      Ic.  
  | Cil.Set ((Mem em, off), e, _) :: instrs ->
      Ic.
  | Cil.Set ((Var v, off), Const _, _) :: instrs 
  | instr :: instrs -> (None, None) :: annotate_block theta conc instrs
  | [] -> []

let annotate_block =
  annotate_block (fun (x: Cil.varinfo) -> bot)
    (fun (x: Ctypes.sloc) -> bot)

val annotate_block: Cil.instr list -> (annotated_block * (Cil.varinfo -> cloc))
