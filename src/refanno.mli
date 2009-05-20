type cloc = int

type ctab = Cil.varinfo cloc Hashtbl.t

type refgen = Ctypes.sloc
type refinst = cloc * Ctypes.sloc

(* annotations precede corresponding instr *) 
type annotated_block = ((refgen option) * (refinst option)) list

(* input: instruction list (of size n) and corresponding ctypes
 * output: list of annotations (of size n) and map from variables to
 * concrete locations *)
val annotate_block: ctab -> Inferctypes.ctemap -> Cil.instr list ->
  (annotated_block * ctab)
