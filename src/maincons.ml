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

(**********************************************************************)
(* API *)
let cons_create ts sm ps cs ws =
  let cs  = FixConstraint.validate cs in
  let sri = Cindex.create cs in
  (sri, ws) 

(* API *)
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


(********************************************************************************)
(****************** TBD: CIL Prepasses, Move into another module ****************)
(********************************************************************************)

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
            Rmtmps.removeUnusedTemps cil;
            CilMisc.purify cil;
            mk_cfg cil;
            rename_locals cil in
  cil

let quals_of_file fname =        
    try
      let qs =
        (fname ^ ".hquals")
        |> open_in 
        |> Lexing.from_channel
        |> FixParse.defs FixLex.token in
      let qs = Misc.map_partial (function C.Qul p -> Some p | _ -> None) qs in
      let _ = Constants.bprintf mydebug "Read Qualifiers: \n%a"
                (Misc.pprint_many true "" Ast.Qualifier.print) qs in
      qs
    with Sys_error s ->
      E.warn "Error reading qualifiers: %s@!@!Continuing without qualifiers...@!@!" s;
      []

let add_spec spec fname =
  let _ = E.log "Parsing spec: %s \n" fname in
  try
    open_in fname
    |> Lexing.from_channel
    |> RefParse.specs RefLex.token
    |> List.fold_left (fun sm (x,y) -> SM.add x y sm) spec 
  with Sys_error s ->
    E.warn "Error reading spec: %s@!@!Continuing without spec...@!@!" s;
    spec

let spec_of_file fname =
  [Constants.lib_name; fname]
  |> List.map (fun s -> s^".spec")
  |> List.fold_left add_spec SM.empty

(********************************************************************************)

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

let print_header () = 
  Printf.printf " \n \n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  Printf.printf "© Copyright 2009 Regents of the University of California.\n";
  Printf.printf "All Rights Reserved.\n"

let mk_options toolname () =
  let us = "Usage: "^toolname^" <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> Constants.file := Some s) us in
  match !Constants.file with
  | Some fn -> fn
  | None    -> assertf "Bug: No input file specified!"

let main f =
  () |> print_header 
     |> ignore
     |> mk_options "maincons.opt"
     |> f 

let _ = main conswrite 
