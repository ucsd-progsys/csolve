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

module F   = Format
module Ct  = Ctypes
module SM  = Misc.StringMap
module SLM = Sloc.SlocMap

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

let unroll_ciltype t =
  match Cil.unrollType t with
  | TComp (ci, _) -> asserti ci.cstruct "TBD conv_cilblock: unions";
                     List.map (fun x -> x.ftype) ci.cfields
  | t             -> [t]

let ctype_of_cilbasetype = function 
  | TVoid _      -> Ct.CTInt (0, Ct.ITop)
  | TInt (ik, _) -> Ct.CTInt (bytesSizeOfInt ik, Ct.ITop)
  | _            -> assertf "ctype_of_cilbasetype: non-base!"

let rec conv_ciltype loc (th, st) t = 
  match t with
  | TVoid _ | TInt (_,_) -> 
      (th, st), ctype_of_cilbasetype t
  | TPtr (t,_) | TArray (t,_,_) ->
      let tid = id_of_ciltype t in
      if SM.mem tid th then (th, st), Ct.CTRef (SM.find tid th, Ct.IInt 0) else
        let l'             = Sloc.fresh Sloc.Abstract in
        let th'            = SM.add tid l' th in
        let (th'', st'), b = conv_cilblock loc (th', st) t in 
        let st''           = SLM.add l' b st' in
        (th'', st''), Ct.CTRef (l', Ct.IInt 0)
  
  | _          -> 
      let _ = errorLoc loc "TBD: conv_ciltype: %a \n\n" d_type t in
      assertf "TBD: conv_ciltype" 

and conv_cilblock loc (th, st) t =
  let ts = t |> unroll_ciltype in
  let _  = Pretty.printf "conv_cilblock: unroll %a \n" d_type t in
  let _  = List.map (fun t' -> Pretty.printf "conv_cilblock: into %a \n" d_type t') ts in
  ts |> Misc.mapfold (conv_ciltype loc) (th, st) 
     |> Misc.app_snd ldesc_of_ctypes 

let slocs_of_store st = 
  SLM.fold (fun l _ locs -> l :: locs) st []

let cfun_of_fundec loc fd = 
  match fd.svar.vtype with 
  | TFun (t, xtso, _, _) -> 
      let xts          = Cil.argsToList xtso in
      let (_,ist), ts  = Misc.map snd3 xts
                         |> Misc.mapfold (conv_ciltype loc) (SM.empty, SLM.empty) in
      let args         = List.map2 (fun (x,_,_) t -> (x,t)) xts ts in
      let (_,ost), ret = conv_ciltype loc (SM.empty, ist) t in
      let qlocs        = slocs_of_store ost in
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
