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

module CM = CilMisc
module VM = CM.VarMap
module Sy = Ast.Symbol
module FI = FixInterface
module SM = Misc.StringMap
module ST = Ssa_transform
module Ix = Ctypes.Index

open Misc.Ops

(***************************************************************************)
(************************ Generate Scalar Constraints **********************)
(***************************************************************************)

let generate cil spec tgr gnv scim : Consindex.t =
  ([], [], [])
  |> Consindex.create  
  |> ConsVisitor.cons_of_scis tgr gnv FI.refstore_empty scim None

(***************************************************************************)
(*************************** Solve Scalar Constraints **********************)
(***************************************************************************)

let solve ci : Ix.t VM.t = 
  Constants.get_lib_squals () 
  |> FI.quals_of_file 
  |> Misc.flip (Consindex.solve ci) "scalar"
  >| (Errormsg.log "TODO: scalar_soln_of_fix_soln \n"; VM.empty)

(***************************************************************************)
(*********************************** API ***********************************)
(***************************************************************************)

let scalarinv_of_scim cil spec tgr gnv =
  generate cil spec tgr gnv <+> solve
