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

(* Checks whether a given set of C files contain (mutually) recursive functions
 * *)
module SM = Misc.StringMap

open Cil
open Misc.Ops

let mydebug = false 

(**********************************************************)
(**************** Check for Cycles ************************)
(**********************************************************)

let self_cycle funt' es =
  es |> Misc.map_partial (fun (i,j) -> if i = j then Some i else None) 
     |> Misc.map (Hashtbl.find funt') 
     |> Misc.map (fun s -> Printf.printf "Function %s calls itself" s; s)

let big_sccs funt' es = 
  let fn   = fun i -> Misc.do_catch "big sccs" (Hashtbl.find funt') i in
  Common.scc_rank fn es
  |> List.map (fun (x,y) -> (y,x))
  |> Misc.hashtbl_of_list
  |> Misc.hashtbl_to_list_all
  |> List.filter (fun scc -> List.length scc > 1)
  |> Misc.map (fun scc -> Misc.map fn scc)
  |> Misc.map (fun scc -> Printf.printf "Big SCC: %s" (String.concat ", " scc); scc)

let check_cycle es =
  let funt    = Hashtbl.create 17 in
  let fresh,_ = Misc.mk_int_factory () in
  let es      = es |> Misc.map (Misc.map_pair (Misc.do_memo funt fresh ())) in 
  let funt'   = Misc.hashtbl_invert funt in
  let selfs   = self_cycle funt' es in
  let sccs    = big_sccs funt' es in
  selfs <> [] || sccs <> []

(******************************************************************)
(********************* Top-Level Build and Check ******************)
(******************************************************************)

let do_main () =
  Printf.printf "© Copyright 2009 Regents of the University of California. ";
  Printf.printf "All Rights Reserved.\n";
  let fs = ref [] in
  let us = "Usage: reccheck <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> fs := s::!fs) us in
  match !fs with
  | []    -> assertf "Bug: No input file specified!"
  | files -> files |> CilMisc.callgraph_of_files |> check_cycle

let _ = 
  if do_main () then
    (Format.printf "\nRECURSION: YES\n"; exit 1)
  else
    (Format.printf "\nRECURSION: NO\n"; exit 0)
