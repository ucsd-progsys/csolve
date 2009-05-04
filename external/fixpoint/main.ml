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

module SM = Ast.Symbol.SMap
module Co = Constants 
module C  = Constraint
module S  = Solve
module F  = Format

open Misc.Ops


(*****************************************************************)
(********************* Command line options **********************)
(*****************************************************************)

let usage = "Usage: liquid <options> [source-files]\noptions are:"

(* taken from dsolve/liquid/liquid.ml *)

let arg_spec = 
  [("-save", 
    Arg.String (fun s -> Co.save_file := s), 
    "Save constraints to file [out]"); 
   ("-drconstr", 
    Arg.Set Co.dump_ref_constraints, 
    "Dump refinement constraints [false]");
   ("-psimple", 
    Arg.Set Co.psimple, 
    "prioritize simple constraints [true]");
   ("-dgraph", 
    Arg.Set Co.dump_graph, 
    "dump constraints SCC to constraints.dot [false]");
   ("-v", Arg.Int (fun c -> Co.verbose_level := c), 
              "<level> Set degree of analyzer verbosity:\n\
               \032    0      No output\n\
               \032    1      +Verbose errors\n\
               \032    [2]    +Verbose stats, timing\n\
               \032    3      +Print normalized source\n\
               \032    11     +Verbose solver\n\
               \032    13     +Dump constraint graph\n\
               \032    64     +Drowning in output") 
  ]


let sift xs = 
  List.fold_left 
    (fun (ts, ps, cs, s) -> function 
      | C.Srt t       -> (t::ts, ps, cs, s) 
      | C.Axm p       -> (ts, p::ps, cs, s) 
      | C.Cst c       -> (ts, ps, c::cs, s)
      | C.Sol (x, qs) -> (ts, ps, cs, SM.add x qs s))
    ([], [], [], SM.empty) xs

let parse f = 
  let _ = Errormsg.startFile f in
  open_in f 
  |> Lexing.from_channel 
  |> FixParse.defs FixLex.token

let solve (ts, ps, cs, s) = 
  let ctx     = S.create ts SM.empty ps cs in
  let s', cs' = S.solve ctx s in
  let _       = S.save !Co.save_file ctx s' in
  F.printf "%a" C.print_soln s'; 
  F.printf "Unsat Constraints :\n %a" 
    (Misc.pprint_many true "\n" (C.print None)) cs';
  ()

let main () =
  Printf.printf "© Copyright 2007 Regents of the University of California. ";
  Printf.printf "All Rights Reserved.\n";
  let fs = ref [] in
  let _  = Arg.parse arg_spec (fun s -> fs := s::!fs) usage in
  !fs |> Misc.tr_flap parse |> sift |> solve 

let _ = main ()
