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

let rename_locals cil =
  Cil.iterGlobals cil
  (function Cil.GFun(fd,_) -> 
    let fn   = fd.Cil.svar.Cil.vname in
    let locs = List.map (fun v -> (v.Cil.vname <- (v.Cil.vname^"@"^fn));v) fd.Cil.slocals in
    let fmls = List.map (fun v -> (v.Cil.vname <- (v.Cil.vname^"@"^fn));v) fd.Cil.sformals in
    fd.Cil.slocals <- locs ;
    fd.Cil.sformals <- fmls
  | _ -> ())

let mk_cfg cil =
  Cil.iterGlobals cil begin function
    | Cil.GFun(fd,_) ->
        Cil.prepareCFG fd;
        Cil.computeCFGInfo fd false
    | _ -> ()
  end

let cil_of_file fname =
  let _   = ignore (E.log "Parsing %s\n" fname) in
  let cil = Frontc.parse fname () |> Simplemem.simplemem in
  let _   = Psimplify.simplify cil;
            Pheapify.heapifyNonArrays := true;
            Pheapify.default_heapify cil;
            Simpleret.simpleret cil;
            Rmtmps.removeUnusedTemps cil;
            CilMisc.purify cil;
            mk_cfg cil;
            rename_locals cil in
  cil

let add_quals quals fname =
    try
      let qs =
        fname
        |> open_in 
        |> Lexing.from_channel
        |> FixParse.defs FixLex.token in
      let qs = Misc.map_partial (function C.Qul p -> Some p | _ -> None) qs in
      let _ = Constants.bprintf mydebug "Read Qualifiers: \n%a"
                (Misc.pprint_many true "" Ast.Qualifier.print) qs in
      qs @ quals
    with Sys_error s ->
      E.warn "Error reading qualifiers: %s@!@!Continuing without qualifiers...@!@!" s;
      quals

let quals_of_file fname =
  [Constants.lib_name; fname]
  |> List.map (fun s -> s^".hquals")
  |> List.fold_left add_quals []

let add_spec spec fname =
  let _ = E.log "Parsing spec: %s \n" fname in
  try
    open_in fname
    |> Lexing.from_channel
    |> RefParse.specs RefLex.token
    |> List.fold_left (fun sm (x,y,b) -> SM.add x (y,b) sm) spec 
  with Sys_error s ->
    E.warn "Error reading spec: %s@!@!Continuing without spec...@!@!" s;
    spec

let spec_of_file fname =
  [Constants.lib_name; fname]
  |> List.map (fun s -> s^".spec")
  |> List.fold_left add_spec SM.empty

let liquidate file =
  let cil   = BS.time "Parse: source" cil_of_file file in
  let _     = E.log "DONE: cil parsing \n" in
  let qs    = BS.time "Parse: quals" quals_of_file file in
  let _     = E.log "DONE: qualifier parsing \n" in
  let spec  = BS.time "Parse: spec" spec_of_file file in
  let _     = E.log "DONE: spec parsing \n" in
  let me    = BS.time "Cons: Generate" (Consgen.create cil) spec in
  let ws    = Consindex.get_wfs me in
  let cs    = Consindex.get_cs me in
  let _     = E.log "DONE: constraint generation \n" in
  let ctx,s = BS.time "Qual Inst" (Solve.create FixInterface.sorts A.Symbol.SMap.empty [] 0 cs ws) qs in
  let _     = E.log "DONE: qualifier instantiation \n" in
  let _     = BS.time "save in" (Solve.save (file^".in.fq") ctx) s in
  let s',cs'= BS.time "Cons: Solve" (Solve.solve ctx) s in 
  let _     = BS.time "save out" (Solve.save (file^".out.fq") ctx) s' in
  let _     = Pretty.printf "%a" (Consindex.print (Some s')) me in
  (cs' = [])

let print_header () = 
  Pretty.printf " \n \n";
  Pretty.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  Pretty.printf "© Copyright 2009 Regents of the University of California.\n";
  Pretty.printf "All Rights Reserved.\n"

let mk_options () =
  let us = "Usage: liquidc <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> Constants.file := Some s) us in
  match !Constants.file with
  | Some fn -> fn
  | None    -> assertf "Bug: No input file specified!"

let main () =
  let _  = print_header () in
  let f  = mk_options () in
  let rv = liquidate f in 
  let _  = BS.print stdout "\nLiquidC Time \n" in
  if rv then begin
    Pretty.printf "\nSAFE\n" |> ignore;
    exit 0
  end else begin
    Pretty.printf "\nUNSAFE\n" |> ignore;
    exit 1
  end

let _ = main ()
