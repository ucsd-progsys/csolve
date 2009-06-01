type ctab 
val cloc_of_varinfo: ctab -> Cil.varinfo -> Ctypes.sloc option (* CLoc *)

type annotation = 
  | Gen of Ctypes.sloc * Ctypes.sloc      (* CLoc s , ALoc s' *)
  | Ins of Ctypes.sloc * Ctypes.sloc      (* ALoc s', CLoc s  *)
  | New of Ctypes.sloc * Ctypes.sloc      (* Aloc s', Cloc s  *) 

(* 1. block_annotation length = block length + 1,
 * 2. annotations precede corresponding instr 
 * 3. n + 1th annotation list corresponds to generalizations at end of block *) 
type block_annotation = annotation list list

(* val d_block_annotation: unit -> block_annotation -> Pretty.doc *)
val d_block_annotation_array: unit -> block_annotation array -> Pretty.doc 

val d_ctab: unit -> ctab -> Pretty.doc 

(* input: cfg with n blocks of length l_i ... l_n
 * output: array of block annotations of length l_i ... l_n
 *         map from variable names to concrete locations *)
val annotate_cfg: Ssa.cfgInfo -> Inferctypes.ctemap -> block_annotation array * ctab
