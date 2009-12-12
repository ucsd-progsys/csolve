(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
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


(** read a set of constraints, solve, and dump out the solution *)

module BS = BNstats
module SM = Ast.Symbol.SMap
module Co = Constants 
module C  = FixConstraint
module S  = Solve
module F  = Format

open Misc.Ops


(*****************************************************************)
(********************* Command line options **********************)
(*****************************************************************)

let sift xs = 
  List.fold_left begin fun (ts, ps, cs, ws, ds, qs, s) -> 
      function 
      | C.Srt t        -> (t::ts, ps, cs, ws, ds, qs, s) 
      | C.Axm p        -> (ts, p::ps, cs, ws, ds, qs, s) 
      | C.Cst c        -> (ts, ps, c::cs, ws, ds, qs, s)
      | C.Wfc w        -> (ts, ps, cs, w::ws, ds, qs, s)
      | C.Dep d        -> (ts, ps, cs, ws, d::ds, qs, s)
      | C.Qul q        -> (ts, ps, cs, ws, ds, q::qs, s)
      | C.Sol (k, kps) -> (ts, ps, cs, ws, ds, qs, SM.add k kps s)
  end ([], [], [], [], [], [], SM.empty) xs

let parse f = 
  let _  = Errorline.startFile f in
  let ic = open_in f in
  let rv = Lexing.from_channel ic |> FixParse.defs FixLex.token in
  let _  = close_in ic in
  rv

let read_inputs usage = 
  print_now "© Copyright 2009 Regents of the University of California. ";
  print_now "All Rights Reserved.\n";
  let fs = ref [] in
  let _  = Arg.parse Co.arg_spec (fun s -> fs := s::!fs) usage in
  let _  = print_now "Fixpoint: Parsing \n" in
  let fq = BS.time "parse" (Misc.flap parse) !fs |> sift in 
  (!fs, fq)

(*****************************************************************)
(********************* Hooking into Solver ***********************)
(*****************************************************************)

let solve (ts, ps, cs, ws, ds, qs, s0) = match cs with 
  | []   -> 
      print_now "Fixpoint: NO Constraints!" 
      |> fun _ -> []

  | c::_ -> 
      let _       = print_now "Fixpoint: Creating  CI\n" in
      let a       = c |> C.tag_of_t |> List.length in
      let ctx,s1  = BS.time "create" (S.create ts SM.empty ps a ds cs ws) qs in
      let _       = print_now "Fixpoint: Solving \n" in
      let s', cs' = BS.time "solve" (S.solve ctx) (C.sol_merge s0 s1) in
      let _       = print_now "Fixpoint: Saving Result \n" in
      let _       = BS.time "save" (S.save !Co.save_file ctx) s' in
      let _       = F.printf "%a \nUnsat Constraints:\n %a" 
                      C.print_soln s' 
                      (Misc.pprint_many true "\n" (C.print_t None)) cs' in
      cs'


let usage = "Usage: fixpoint <options> [source-files]\noptions are:"

let main () =
  let cs' = read_inputs usage |> snd |> BS.time "solve" solve in
  let _   = BNstats.print stdout "Fixpoint Solver Time \n" in
  match cs' with 
  | [] -> (F.printf "\nSAT\n" ; exit 0)
  | _  -> (F.printf "\nUNSAT\n" ; exit 1)

let _ = main ()
