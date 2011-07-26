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

module FA  = FixAstInterface
module Ct  = Ctypes
module Co  = Constants
module RCt = Ct.RefCTypes
module Sc  = ScalarCtypes
module PP  = Pretty
module SM  = Misc.StringMap

open Misc.Ops

let mydebug = false

(*******************************************************************)
(****************** Tag/Annotation Generation **********************)
(*******************************************************************)

type binding = TVar of FA.name * Ct.refctype
             | TFun of string  * Ct.refcfun
             | TSto of string  * Ct.refstore

let report_bad_binding = function 
  | TVar (x, cr) ->
      Errormsg.warn "\nBad TVar for %s :: \n\n@[%a@]" (FA.string_of_name x) Ct.d_refctype cr
  | TFun (fn, cf) ->
      Errormsg.warn "\nBad TFun for %s ::\n\n@[%a@]" fn Ct.d_refcfun cf
  | TSto (fn, st) -> 
      Errormsg.error "\nBad TSto for %s ::\n\n@[%a@]" fn Ct.d_refstore st 

let tags_of_binds s binds = 
  let s_typ = RCt.CType.map (Misc.app_snd (FixConstraint.apply_solution s)) in
  let s_fun = RCt.CFun.map s_typ in
  let s_sto = RCt.Store.map s_typ in
  let nl    = Constants.annotsep_name in
  List.fold_left begin fun (d, kts) bind -> 
    try
      match bind with 
      | TVar (n, cr) ->
          let x    = FA.string_of_name n in
          let k,t  = x, ("variable "^x) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t Ct.d_refctype (s_typ cr) nl in
          (Pretty.concat d d', (k,t)::kts)
      | TFun (f, cf) -> 
          let k,t  = f, ("function "^f) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t Ct.d_refcfun (s_fun cf) nl in
          (Pretty.concat d d', (k,t)::kts)
      | TSto (f, st) -> 
        let kts' =  RCt.Store.domain st 
                 |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
                 |> List.map (fun s -> (s, s^" |->")) in
        let d'   = Pretty.dprintf "funstore %s ::\n\n@[%a@] %s" f Ct.d_refstore (s_sto st) nl in
        (Pretty.concat d d', kts' ++ kts)
    with
      FixSolution.UnmappedKvar _ -> (if mydebug then report_bad_binding bind); (d, kts)
  end (Pretty.nil, []) binds

let generate_annots d = 
  let fn = !Co.liquidc_file_prefix ^ ".annot" in
  let oc = open_out fn in
  let _  = Pretty.fprint ~width:80 oc d in
  let _  = close_out oc in
  ()

let generate_tags kts =
  let fn = !Co.liquidc_file_prefix ^ ".tags" in
  let oc = open_out fn in
  let _  = kts |> List.sort (fun (k1,_) (k2,_) -> compare k1 k2) 
               |> List.iter (fun (k,t) -> ignore <| Pretty.fprintf oc "%s\t%s.annot\t/%s/\n" k !Co.liquidc_file_prefix t) in
  let _  = close_out oc in
  ()

(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

(* UGH. Global State. *)
let annotr    = ref [] 

(* API *)
let annot_var x cr = annotr := TVar (x, cr) :: !annotr
let annot_fun f cf = annotr := TFun (f, cf) :: !annotr
let annot_sto f st = annotr := TSto (f, st) :: !annotr
let clear _        = annotr := []

(* API *)
let dump s = 
  !annotr
  |> tags_of_binds s 
  >> (fst <+> generate_annots)
  >> (snd <+> generate_tags) 
  |> ignore

(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

(*
let d_vartyp () (v, t) = 
  PP.dprintf "(%s :: %a)" v.Cil.vname Cil.d_type v.Cil.vtype

let d_vartypes () vts = 
  PP.seq (PP.text ",") (d_vartyp ()) vts

let d_sloc_vartyps () (sloc, vts) = 
  PP.dprintf "[%a |-> %a]\n" Sloc.d_sloc sloc d_vartypes vts
*)

let d_vars () vs = 
  PP.docList ~sep:(PP.text ",") (fun v -> PP.dprintf "%s" v.Cil.vname) () vs

let d_typ_vars () (t, vs) = 
  PP.dprintf "%a %a;@!" Cil.d_type t d_vars vs

let d_typ_varss () tvss =
  PP.docList ~sep:(PP.dprintf "@!") (d_typ_vars ()) () tvss 

let d_sloc_typ_varss () (sloc, tvss) = 
  PP.dprintf "%a <<%d>> |-> @[%a@]" 
    Sloc.d_sloc sloc
    (List.length tvss)
    d_typ_varss tvss

(* API *)
let stitch_shapes_ctypes cil shm = 
  Misc.write_to_file (!Constants.liquidc_file_prefix ^ ".shape") "SHAPE INFORMATION";
  SM.iter begin fun fn shp ->
    Misc.kgroupby (snd <+> Ctypes.I.CType.sloc) shp.Shape.vtyps
    |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
    |> List.map (Misc.app_snd (List.map fst))
    |> List.map (Misc.app_snd (Misc.kgroupby (fun v -> v.Cil.vtype)))
    |> PP.docList ~sep:(PP.dprintf "@!") (d_sloc_typ_varss ()) ()
    |> PP.concat (PP.text ("STITCH SHAPE: "^fn^"\n"))
    |> PP.sprint ~width:80
    |> (Misc.append_to_file (!Constants.liquidc_file_prefix ^ ".shape")) 
  end shm
  (* ; E.log "EXIT: stitch_shapes_ctypes"; exit 0 *)
