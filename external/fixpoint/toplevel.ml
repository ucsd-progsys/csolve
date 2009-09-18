module BS = BNstats
module SM = Ast.Symbol.SMap
module Co = Constants 
module C  = FixConstraint
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


