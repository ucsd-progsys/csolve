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

(*
val conv_ciltype: location -> ('a * Cil.typ) -> store -> ('a * ctype) * store

let rec conv_ciltype loc (tlocm store t = 
  match t with
  | TVoid _ 
  | TInt (_,_) -> ctype_of_cilbasetype t
  | TPtr (t,_) -> let tid = id_of_t t in
      try tlocm, store, Ct.CTRef (IM.find tid tlocm, 0) with Not_found ->
        let l' = fresh_location () in
        let tlocm', st', b' = conv_cilblock loc (IM.add tid l') store b in
        tlocm', store[l' -> b'], REF(l',0)


and conv_cilblock loc tlocm store ts =


let conv_ciltype loc (x, t) st = 
  let _, st', ct' = conv_ciltype loc empty t st in
  (x, ct') st'
*)


let ctype_of_cilbasetype = function 
  | TVoid _      -> Ct.CTInt (0, Ct.ITop)
  | TInt (ik, _) -> Ct.CTInt (bytesSizeOfInt ik, Ct.ITop)
  | _            -> assertf "ctype_of_cilbasetype: non-base!"

(* {{{
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
}}} *)

let cfun_of_fundec loc fd = 
  match fd.svar.vtype with 
  | TFun (t, xtso, _, _) -> 
      let emp          = Sloc.SlocMap.empty in
      let args, ist    = xtso |> Cil.argsToList 
                              |> Misc.mapfold (conv_ciltype loc) emp in
      let (_,ret), ost = conv_ciltype loc ("ret", t) ist in
      let qlocs        = locs_of_store ost in
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
  let oc = open_out (fname^".autospec") in
  Frontc.parse fname ()
  |> specs_of_file
  |> List.iter (fun (fn, cf) -> Pretty.fprintf oc "%s :: @[%a@] \n\n" fn Ctypes.d_cfun cf |> ignore)
  |> fun _ -> close_out oc 

let _ = Toplevel.main "genspec.opt" mk_spec
