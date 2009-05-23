type ctab 

val cloc_of_varinfo: ctab -> Cil.varinfo -> Ctypes.sloc (* CLoc *)

type annotation = 
  | Gen  of Ctypes.sloc * Ctypes.sloc      (* CLoc c, ALoc s *)
  | Inst of Ctypes.sloc * Ctypes.sloc      (* ALoc s, CLoc c *)
type block_annotation = annotation list list
(* annotations precede corresponding instr *) 

val d_block_annotation: unit -> block_annotation -> Pretty.doc
val d_ctab: unit -> ctab -> Pretty.doc 

(* input: cfg with n blocks of length l_i ... l_n
 * output: array of block annotations of length l_i ... l_n
 *         map from variable names to concrete locations *)
val annotate_cfg: Ssa.cfgInfo -> Inferctypes.ctemap -> block_annotation array * ctab
