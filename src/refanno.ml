

open Cil

type cloc = int

let bot = 0

type annotation =
  RefGen of Ctypes.sloc | RefInst of cloc * Ctypes.sloc

let fresh_cloc =
  let i = ref bot in
  (fun () -> incr i; !i)

type annotated_block = (annotation option * annotation option) list

let annotate_block instrs theta conc =

let annotate_block instrs = annotate_block (fun _ -> 

val annotate_block: Cil.instr list -> (annotated_block * (Cil.varinfo -> cloc))
