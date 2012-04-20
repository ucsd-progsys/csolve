(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(* This file is part of the liquidC Project.*)

type tag
type tagm = tag Sloc.SlocMap.t
type cncm = tagm Sloc.SlocMap.t
type ctab 

type annotation = 
  | Gen  of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | WGen of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | Ins  of string * Sloc.t * Sloc.t    (* ptr var, ALoc, CLoc *)
  | New  of Sloc.t * Sloc.t             (* Xloc, Yloc *) 
  | NewC of Sloc.t * Sloc.t * Sloc.t    (* XLoc, Aloc, CLoc *) 
  | HInst of Ctypes.StoreSubst.t
  | TNew of Ctypes.tvar * Ctypes.tvar
  | TInst of Ctypes.tvinst

(* 1. block_annotation length = block length,
 * 2. annotations _precede_ corresponding instr 
 * 3. gens for end of block placed on out-edges of block *) 
type block_annotation = annotation list list

val tag_dirty : tag -> bool
val tag_eq : tag -> tag -> bool
val d_conca: unit -> (cncm * cncm) array -> Pretty.doc 
val d_block_annotation_array: unit -> block_annotation array -> Pretty.doc 
val d_ctab: unit -> ctab -> Pretty.doc 

val cloc_of_varinfo: ctab -> Cil.varinfo -> Sloc.t option (* CLoc *)
val clocs_of_aloc  : cncm -> Sloc.t -> Sloc.t list
val subs : Sloc.Subst.t -> block_annotation -> block_annotation

(* input: cfg with n blocks of length l_i ... l_n
 * output: 1. array of block annotations of length l_i ... l_n
 *         2. map from edges (i,j) to (gen)-annots for that edge
 *         3. map from variable names to concrete locations *)
val annotate_cfg: Ssa.cfgInfo -> 
                  Sloc.t list -> 
                  Ctypes.ctemap -> 
                  block_annotation array -> 
		  block_annotation array * (cncm * cncm) array * ctab

(*
   1. A : block * block -> annot list
   2. B : block -> annot list list
   3. Co: block -> abs-loc -> conc-loc
   4. Ci: block -> abs-loc -> conc-loc
   
   Upd: locmap -> annot list -> locmap

   - INVARIANT 1:
     forall blocks i, 
        Upd(Ci(i), B(i)) = Co(i)
   
   - INVARIANT 2:
     forall blocks i, forall j in preds(j),
        - Ci(i) = Upd(Co(j), A(j,i)) 
        - Ci(i)(L) = l => Co(j)(L) = l && CGen(l,L) in A(j,i)
   
   RECONSTRUCTION
   - From A and B we can reconstruct rest.
   - From Ci and B we can reconstruct rest.
*)
