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

type indextyping   = (Ctypes.cfun * Ctypes.ctype CilMisc.VarMap.t) CilMisc.VarMap.t
type dcheck        = Cil.varinfo * FixInterface.refctype
type block_dchecks = dcheck list list

val d_blocks_dchecks : unit -> block_dchecks array -> Pretty.doc

val infer_fun_indices :
  Ctypes.cfun CilMisc.VarMap.t ->
  Ctypes.ctype CilMisc.VarMap.t ->
  Ssa_transform.ssaCfgInfo CilMisc.VarMap.t ->
  Ctypes.cfun ->
  Ssa_transform.ssaCfgInfo ->
  Ctypes.ctype CilMisc.VarMap.t * block_dchecks array
