type cloc = int

type ctab = (Cil.varinfo, cloc) Hashtbl.t

type refgen = Ctypes.sloc
type refinst = Ctypes.sloc * cloc
type annotation = (refgen option) * (refinst option)

(* annotations precede corresponding instr *) 
type block_annotation = annotation list

(* input: instruction list (of size n) and corresponding ctypes
 * output: list of annotations (of size n) and map from variables to
 * concrete locations *)
val annotate_block: ctab -> Inferctypes.ctemap -> Cil.instr list ->
  (block_annotation * ctab)

(*val annotate_cfg: Cil.cfgInfo ->*)
