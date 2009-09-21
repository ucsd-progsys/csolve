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

let byte_size_of_cil : Cil.typ -> int  = failwith "TBD: byte_size_of_cil"
let is_recursive_cil : Cil.typ -> bool = failwith "TBD: is_recursive_cil"
let idx_of_cil c i =
  if is_recursive_cil c 
  then (Ct.IInt i)
  else (Ct.ISeq (i, byte_size_of_cil c))

let ldesc_of_ctypes cs ts =
  let _      = asserti (ts <> []) "ERROR: ldesc_of_types" in
  let _      = asserti (List.length ts = List.length cs) "ERROR: ldesc" in
  let c, _   = Misc.list_snoc cs in
  let t, ts  = Misc.list_snoc ts in
  let i, its = Misc.mapfold (fun i t -> (i + Ct.prectype_width t), (Ct.IInt i,t)) 0 ts in
  let idx    = match c with TPtr (c,_) | TArray (c,_,_) -> idx_of_cil c i 
               | _ -> Ct.IInt 0 in
  its @ [(idx, t)]
  |> Ct.LDesc.create

let unroll_ciltype t =
  match Cil.unrollType t with
  | TComp (ci, _) -> asserti ci.cstruct "TBD unroll_ciltype: unions";
                     List.map (fun x -> x.ftype) ci.cfields
  | t             -> [t]

let ctype_of_cilbasetype = function 
  | TVoid _      -> Ct.CTInt (0, Ct.ITop)
  | TInt (ik, _) -> Ct.CTInt (bytesSizeOfInt ik, Ct.ITop)
  | _            -> assertf "ctype_of_cilbasetype: non-base!"

let rec conv_ciltype loc (th, st) c = 
  match c with
  | TVoid _ | TInt (_,_) -> 
      (th, st), ctype_of_cilbasetype c
  | TPtr (c,_) | TArray (c,_,_) ->
      let tid = id_of_ciltype c in
      let idx = idx_of_cil c 0 in
      if SM.mem tid th then
        let l              = SM.find tid th in
        (th, st), Ct.CTRef (l, idx) 
      else
        let l              = Sloc.fresh Sloc.Abstract in
        let th'            = SM.add tid l th in
        let (th'', st'), b = conv_cilblock loc (th', st) c in 
        let st''           = SLM.add l b st' in
        (th'', st''), Ct.CTRef (l, idx)
  | _          -> 
      let _ = errorLoc loc "TBD: conv_ciltype: %a \n\n" d_type c in
      assertf "TBD: conv_ciltype" 

and conv_cilblock loc (th, st) c =
  let cs = c |> unroll_ciltype in
  let _  = Pretty.printf "conv_cilblock: unroll %a \n" d_type c in
  let _  = List.map (fun c' -> Pretty.printf "conv_cilblock: into %a \n" d_type c') cs in
  cs |> Misc.mapfold (conv_ciltype loc) (th, st) 
     |> Misc.app_snd (ldesc_of_ctypes cs) 

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
