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

(* Top-level spec parser *)

module SM = Misc.StringMap
module FI = FixInterface
open Misc.Ops

let mydebug = false 

let read_spec files : FI.refcfun SM.t =
  List.fold_left begin fun sm f ->
    (f (* ^ ".spec" *) ) 
    |> open_in
    |> Lexing.from_channel
    |> RefParse.specs RefLex.token
    |> List.fold_left (fun sm (x,y) -> SM.add x y sm) sm
  end SM.empty files

let print_spec sm = 
  Format.printf "spec parsed: OK \n";
  SM.iter begin fun s rft -> 
    let ft = FI.cfun_of_refcfun rft in
    Errormsg.log "%s :: %a \n" s (Ctypes.d_precfun Ctypes.d_index) ft
  end sm

let _ =
  Printf.printf "© Copyright 2009 Regents of the University of California. ";
  Printf.printf "All Rights Reserved.\n";
  let fs = ref [] in
  let us = "Usage: specparse <options> [source-file] \n options are:" in
  let _  = Arg.parse Constants.arg_spec (fun s -> fs := s::!fs) us in
  match !fs with
  | []    -> assertf "Bug: No input file specified!"
  | files -> read_spec files |> print_spec
