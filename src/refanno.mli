type cloc = string 

type ctab (*= (string, cloc) Hashtbl.t*)

(* throws Not_found *)
val cloc_of_v_pub: ctab -> Cil.varinfo -> cloc

type refgen = Ctypes.sloc
type refinst = Ctypes.sloc * cloc
type annotation = (refgen option) * (refinst option)

(* annotations precede corresponding instr *) 
type block_annotation = annotation list

(* input: cfg with n blocks of length l_i ... l_n
 * output: array of block annotations of length l_i ... l_n
 *         map from variable names to concrete locations *)
val annotate_cfg: Ssa.cfgInfo -> Inferctypes.ctemap -> block_annotation array * ctab
