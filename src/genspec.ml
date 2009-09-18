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
module SM = Misc.StringMap

open Cil
open Misc.Ops

exception NoSpec

let id_of_ciltype t : string = 
  t |> Cil.typeSig 
    |> Cil.d_typsig () 
    |> Pretty.sprint ~width:80

let ldesc_of_ctypes (ts: Ct.ctype list) : Ct.index Ct.LDesc.t =
  ts|> Misc.mapfold (fun i t -> (i + Ct.prectype_width t), (Ct.index_of_int i,t)) 0  
    |> snd |> Ct.LDesc.create 

let unroll_ciltype = function
  | TComp (ci, _) -> asserti ci.cstruct "TBD conv_cilblock: unions";
                     List.map (fun x -> x.ftype) ci.cfields
  | t             -> [t]

let ctype_of_cilbasetype = function 
  | TVoid _      -> Ct.CTInt (0, Ct.ITop)
  | TInt (ik, _) -> Ct.CTInt (bytesSizeOfInt ik, Ct.ITop)
  | _            -> assertf "ctype_of_cilbasetype: non-base!"

let rec conv_ciltype loc (th, st) t = 
  match t with
  | TVoid _ 
  | TInt (_,_) -> 
      (th, st), ctype_of_cilbasetype t
  | TPtr (t,_) -> 
      let tid = id_of_ciltype t in
      try (th, st), Ct.CTRef (SM.find tid th, Ct.IInt 0) with Not_found ->
        let l'             = Sloc.fresh Sloc.Abstract in
        let th'            = SM.add tid l' th in
        let (th'', st'), b = conv_cilblock loc (th', st) t in 
        let st''           = Sloc.SlocMap.add l' b st' in
        (th'', st''), Ct.CTRef (l', Ct.IInt 0)
  | _          -> 
      assertf "TBD: conv_ciltype"

and conv_cilblock loc (th, st) t =
  t |> unroll_ciltype 
    |> Misc.mapfold (conv_ciltype loc) (th, st) 
    |> Misc.app_snd ldesc_of_ctypes 

let cfun_of_fundec loc fd = 
  match fd.svar.vtype with 
  | TFun (t, xtso, _, _) -> 
      let emp = Sloc.SlocMap.empty in
      let xts = Cil.argsToList xtso in
      let (_,ist), ts  = Misc.map snd3 xts
                         |> Misc.mapfold (conv_ciltype loc) (SM.empty, emp) in
      let args         = List.map2 (fun (x,_,_) t -> (x,t)) xts ts in
      let (_,ost), ret = conv_ciltype loc (SM.empty, ist) t in
      let qlocs        = ost |> Ct.prestore_slocset |> Sloc.SlocSet.elements in
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
