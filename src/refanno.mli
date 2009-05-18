type cloc

val cbot: cloc

type annotation =
  RefGen of Ctypes.sloc | RefInst of cloc * Ctypes.sloc

(* tuple denotes (before, after) instruction *) 
type annotated_block = (annotation option * annotation option) list

(* takes an instruction list and returns a list of annotations of identical
 * size *)
val annotate_block: Inferctypes.ctemap -> Cil.instr list -> (annotated_block * (Cil.varinfo -> cloc))
