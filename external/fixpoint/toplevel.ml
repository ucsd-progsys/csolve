module BS = BNstats
module SM = Ast.Symbol.SMap
module Co = Constants 
module C  = FixConstraint
module F  = Format
open Misc.Ops


(*****************************************************************)
(********************* Command line options **********************)
(*****************************************************************)

let parse f = 
  let _  = Errorline.startFile f in
  let ic = open_in f in
  let rv = Lexing.from_channel ic |> FixParse.defs FixLex.token in
  let _  = close_in ic in
  rv

let read_inputs usage = 
  print_now "\n \n \n \n \n";
  print_now "========================================================\n";
  print_now "© Copyright 2009 Regents of the University of California.\n";
  print_now "All Rights Reserved.\n";
  print_now "========================================================\n";
  print_now (Sys.argv |> Array.to_list |> String.concat " ");
  print_now "\n========================================================\n";
  let fs = ref [] in
  let _  = Arg.parse Co.arg_spec (fun s -> fs := s::!fs) usage in
  let fq = !fs |> BS.time "parse" (Misc.flap parse) |> FixConfig.create in 
  (!fs, fq)
