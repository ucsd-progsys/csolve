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
module ST = Ssa_transform
module A  = Ast
module C  = Constraint
module SM = Misc.StringMap
module Sy = Ast.Symbol
module P  = Pretty

module I  = Inferctypes 

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
  let _ =
    Cil.foldGlobals cil
      (fun fds -> function
         | (Cil.GFun(fd,_) as fundec) ->
             let _ = fundec in
               Psimplify.doGlobal fundec;
               Inliner.doGlobal fds fundec;
               Cil.prepareCFG fd;
               Cil.computeCFGInfo fd false;
               (fd.Cil.svar.Cil.vid, fd) :: fds
         | _ -> fds) []
  in ()

let mk_cil fname =
  let _   = ignore (E.log "Parsing %s\n" fname) in
  let cil = Frontc.parse fname () |> Simplemem.simplemem in
  let _   = Heapify.heapifyNonArrays := true;
            Heapify.default_heapify cil;
            Rmtmps.removeUnusedTemps cil;
            mk_cfg cil;
            rename_locals cil in
  cil

let mk_quals (f:string) : Ast.Qualifier.t list =        
    try
      let qs =
        open_in f
        |> Lexing.from_channel
        |> FixParse.defs FixLex.token in
      let qs = Misc.map_partial (function C.Qul p -> Some p | _ -> None) qs in
      let _ = Constants.bprintf mydebug "Read Qualifiers: \n%a"
                (Misc.pprint_many true "" Ast.Qualifier.print) qs in
      qs
    with Sys_error s ->
      E.warn "Error reading qualifiers: %s@!@!Continuing without qualifiers...@!@!" s;
      []

let liquidate file =
  let cil   = mk_cil file in
  let qs    = mk_quals (file^".hquals") in
  let me    = Consgen2.create cil in
  let ws    = Consindex.get_wfs me in
  let cs    = Consindex.get_cs me in
  let ctx,s = Solve.create [] A.Symbol.SMap.empty [] cs ws qs in
  let _     = Solve.save (file^".in.fq") ctx s in
  let s',cs'= Solve.solve ctx s in 
  let _     = Solve.save (file^".out.fq") ctx s' in
  let _     = Consindex.print (Some s') Format.std_formatter me in 
  (cs' = [])

let print_header () = 
  Printf.printf " \n \n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  Printf.printf "© Copyright 2009 Regents of the University of California.\n";
  Printf.printf "All Rights Reserved.\n"

let mk_options () =
  let fs = ref [] in
  let us = "Usage: liquidc <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> fs := s::!fs) us in
  match !fs with
  | [fn] -> fn
  | []   -> assertf "Bug: No input file specified!"
  | _    -> assertf "Bug: More than one input file specified!"

let main () = 
  let _ = print_header () in
  let f = mk_options () in
  if liquidate f then print_string "SAFE" else print_string "UNSAFE"

let _ = main ()
