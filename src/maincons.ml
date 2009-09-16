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

module E  = Errormsg
module A  = Ast
module C  = FixConstraint
module SM = Misc.StringMap
module Sy = Ast.Symbol
module P  = Pretty

open Misc.Ops

let mydebug = false 

let cons_create ts sm ps cs ws =
  let cs  = FixConstraint.validate cs in
  let sri = Cindex.create cs in
  (sri, ws) 

let cons_save fname (sri, ws) =
  let oc  = open_out fname in
  let ppf = Format.formatter_of_out_channel oc in
  Cindex.iter  
    (Format.fprintf ppf "@[%a@] \n" (FixConstraint.print_t None))
    sri;
  List.iter
    (Format.fprintf ppf "@[%a@] \n" (FixConstraint.print_wf None))
    ws;
  close_out oc

let conswrite file =
  let cil   = cil_of_file file in
  let _     = E.log "DONE: cil parsing \n" in
  let spec  = spec_of_file file in
  let _     = E.log "DONE: spec parsing \n" in
  let me    = Consgen.create cil spec in
  let ws    = Consindex.get_wfs me in
  let cs    = Consindex.get_cs me in
  let _     = E.log "DONE: constraint generation \n" in
  cons_create FixInterface.sorts A.Symbol.SMap.empty [] cs ws 
  |> cons_save (file^".in.fq") 

let _ = Toplevel.main "maincons.native" conswrite 
