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

module F  = Format
module Ct = Ctypes
open Cil

open Misc.Ops

exception NoSpec

let ctype_of_ciltype loc = function 
  | TVoid _      -> Ct.CTInt (0, Ct.ITop)
  | TInt (ik, _) -> Ct.CTInt (bytesSizeOfInt ik, Ct.ITop)
  | _            -> errorLoc loc "Can't create spec for exotic type" |> ignore; raise NoSpec

let cfun_of_fundec loc fd = 
  match fd.svar.vtype with 
  | TFun (t, xtso, _, _) -> 
      let qlocs = [] in 
      let args  = Cil.argsToList xtso
                  |> List.map (fun (x,y,_) -> (x,y))
                  |> List.map (Misc.app_snd (ctype_of_ciltype loc)) in
      let ret   = ctype_of_ciltype loc t in 
      let ist   = Sloc.SlocMap.empty in
      let ost   = Sloc.SlocMap.empty in
      Ct.mk_cfun qlocs args ret ist ost
  | _ -> 
      let _ = errorLoc loc "Non-fun type for %s\n\n" fd.svar.vname in
      assert false

let specs_of_file cil =
  foldGlobals cil begin fun acc -> function
    | GFun (fd, loc) -> begin 
        try (fd.svar.vname, cfun_of_fundec loc fd) :: acc with NoSpec -> 
          let _ = warnLoc loc "Skipping spec for %s\n\n" fd.svar.vname in 
          acc
      end
    | _ -> acc
  end [] 

let mk_spec fname = 
  let oc = open_out (fname^".spec") in
  Frontc.parse fname ()
  |> specs_of_file
  |> List.iter (fun (fn, cf) -> Pretty.fprintf oc "%s ::@. %a @.@." fn Ctypes.d_cfun cf |> ignore)
  |> fun _ -> close_out oc 

let _ = Toplevel.main "genspec.opt" mk_spec
