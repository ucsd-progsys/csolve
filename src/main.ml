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

module BS = BNstats
module E  = Errormsg
module A  = Ast
module C  = FixConstraint
module SM = Misc.StringMap
module Sy = Ast.Symbol
module P  = Pretty

open Misc.Ops

 (* Pre-passes from blastCilInterface.ml:
  * one return value
  * simplify boolean expressions *)
let mydebug = false 

let location_of_constraint tgr = 
  FixConstraint.tag_of_t <+> CilTag.t_of_tag <+> CilTag.loc_of_t tgr

let print_unsat_locs tgr s ucs =
  let ucs = Misc.fsort (location_of_constraint tgr) ucs in
  List.iter begin fun c ->
    P.printf "\nUnsafe Type at %a:\n\n%a\n" 
      Cil.d_loc (location_of_constraint tgr c) 
      (fun () -> FixConstraint.print_t (Some s) |> CilMisc.doc_of_formatter) c
    |> ignore
  end ucs

let liquidate file =
  let cil   = BS.time "Parse: source" Toplevel.cil_of_file file in
  let _     = E.log "DONE: cil parsing \n" in
  let qs    = BS.time "Parse: quals" Toplevel.quals_of_file file in
  let _     = E.log "DONE: qualifier parsing \n" in
  let spec  = BS.time "Parse: spec" (Toplevel.spec_of_file file) cil in
  let _     = E.log "DONE: spec parsing \n" in
  let tgr,me= BS.time "Cons: Generate" (Consgen.create cil) spec in
  
  let ws    = Consindex.get_wfs me in
  let cs    = Consindex.get_cs me in
  let ds    = Consindex.get_deps me in
  let _     = E.log "DONE: constraint generation \n" in
(*let _     = List.iter (fun w -> Format.printf "%a" (C.print_wf None) w) ws in
  let _     = List.iter (fun c -> Format.printf "%a" (C.print_t None) c) cs in *)
  let ctx,s = BS.time "Qual Inst" (Solve.create FixInterface.sorts A.Symbol.SMap.empty [] 4 ds cs ws) qs in
  let _     = E.log "DONE: qualifier instantiation \n" in
  let _     = BS.time "save in" (Solve.save (file^".in.fq") ctx) s in
  let _     = if !Constants.dump_ref_constraints 
              then (E.log "DONE: constraints in %s.in.fq \n" file; exit 0) in
  let s',cs'= BS.time "Cons: Solve" (Solve.solve ctx) s in 
  let _     = BS.time "save out" (Solve.save (file^".out.fq") ctx) s' in
  let _     = FixInterface.annot_dump file s' in
  
  let _     = print_unsat_locs tgr s' cs' in
  let _  = BS.print stdout "\nLiquidC Time \n" in
  match cs' with 
  | [] -> let _ = P.printf "\nSAFE\n"   in exit 0
  | _  -> let _ = P.printf "\nUNSAFE\n" in exit 1

let main () =
  let _  = Toplevel.print_header () in
  let f  = Toplevel.mk_options "main.native" () in
  let _  = if !Constants.do_nothing then exit 0 in
  if !Constants.genspec then
    f |> Toplevel.spec_of_file |> ignore 
  else
    liquidate f

let _ = main ()
