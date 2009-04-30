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
  F.printf "%a" C.print_soln s'; 
  F.printf "Unsat Constraints :\n %a" (Misc.pprint_many true "\n" (C.print None)) cs';
  ()

let main () =
  Array.to_list Sys.argv |> String.concat " " |> Printf.printf "FixPoint 0.1 $ %s \n" ;
  Printf.printf "© Copyright 2007 Regents of the University of California. ";
  Printf.printf "All Rights Reserved.\n"; 
  (try Sys.argv.(1) with _ -> failure "ERROR: bad inputs") 
  |> parse |> sift |> solve 

let _ = main ()

 
