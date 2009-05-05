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
module W  = Wrapper

open Misc.Ops

 (* Pre-passes from blastCilInterface.ml:
  * one return value
  * simplify boolean expressions *)

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
  Cil.iterGlobals cil 
  (function Cil.GFun(fd,_) as fundec ->
    Simplify.doGlobal fundec; 
    Cil.prepareCFG fd; 
    Cil.computeCFGInfo fd false 
  | _ -> ())

let mk_cil fname =
  let _   = ignore (E.log "Parsing %s\n" fname) in
  let cil = Frontc.parse fname () |> Simplemem.simplemem in
  let _   = Rmtmps.removeUnusedTemps cil; 
            mk_cfg cil;
            rename_locals cil in
  cil

let mk_scis cil = 
  Cil.foldGlobals cil
    (fun acc g ->
      match g with 
      | Cil.GFun (fd,loc) -> 
          let _   = E.log "before fdec_to_ssa \n" in
          let sci = ST.fdec_to_ssa_cfg fd loc in
          let _   = E.log "after fdec_to_ssa \n";
                    ST.print_sci sci in
          sci::acc
      | _ -> acc) [] 

let mk_quals (f:string) : A.pred list =        
  failwith "TBDNOW: parse qualifiers from file" 

let mk_genv (cil: Cil.file) : W.cilenv =                       
  (* TBD: initialize with global variables *)
  W.ce_empty 

(*
let mk_tp_env cil =
  Cil.foldGlobals cil 
    (fun env g -> 
      match g with 
      | Cil.GFun(fd,_) ->
          List.fold_left 
            (fun e v -> 
              let vn = v.Cil.vname in
              let vt = TP.embed_type v.Cil.vtype in
              SM.add vn vt e) 
            env (fd.Cil.sformals ++ fd.Cil.slocals)
      | _ -> env)
    SM.empty 
*)

let liquidate file =
  let cil    = mk_cil file in
  let qs     = mk_quals file in
  let g0     = mk_genv cil in
  let scis   = mk_scis cil in
  let cs,s   = Consgen.mk_cons qs g0 scis in
  let ctx    = Solve.create [] A.Symbol.SMap.empty [] cs in
  let _      = Solve.save (file^".in.fq") ctx s in
  let s',cs' = Solve.solve ctx s in 
  let _      = Solve.save (file^".out.fq") ctx s' in
  (cs' = [])

let print_header () = 
  Printf.printf " \n \n";
  Printf.printf "$ %s \n" (String.concat " " (Array.to_list Sys.argv));
  Printf.printf "© Copyright 2009 Regents of the University of California.";
  Printf.printf "All Rights Reserved.\n"

let mk_options () = 
  let n = ref 1 in
  let _ = 
    while !n < Array.length Sys.argv && 
          String.length Sys.argv.(!n) > 0 && 
          Sys.argv.(!n).[0] = '-' do
      n := !n + 1
    done in
  if (Array.length Sys.argv - !n >= 1) then 
    Sys.argv.(!n) 
  else E.s (E.bug "No input file specified!")

let main () = 
  let _ = print_header () in
  let f = mk_options () in
  if liquidate f then print_string "SAFE" else print_string "UNSAFE"

let _ = main ()
