(*
 * Copyright © 1990-2011 The Regents of the University of California. All rights reserved. 
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
module PP  = Pretty

module SS  = Misc.StringSet
module SM  = Misc.StringMap
module SLM = Sloc.SlocMap
module IM  = Misc.IntMap
open Misc.Ops

let mydebug = false

(* YUCK!!! Global State. *)
let annotr    = ref [] 
let shaper    = ref []

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

let tags_of_binds binds = 
  let nl    = Constants.annotsep_name in
  List.fold_left begin fun (d, kts) bind -> 
    try
      match bind with 
      | TVar (n, cr) ->
          let x    = FA.string_of_name n in
          let k,t  = x, ("variable "^x) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t Ct.d_refctype cr nl in
          (Pretty.concat d d', (k,t)::kts)
      | TFun (f, cf) -> 
          let k,t  = f, ("function "^f) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t Ct.d_refcfun cf nl in
          (Pretty.concat d d', (k,t)::kts)
      | TSto (f, st) -> 
        let kts' =  RCt.Store.domain st 
                 |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
                 |> List.map (fun s -> (s, s^" |->")) in
        let d'   = Pretty.dprintf "funstore %s ::\n\n@[%a@] %s" f Ct.d_refstore st nl in
        (Pretty.concat d d', kts' ++ kts)
    with
      FixConfig.UnmappedKvar _ -> (if mydebug then report_bad_binding bind); (d, kts)
  end (Pretty.nil, []) binds

let generate_annots d = 
  let fn = !Co.liquidc_file_prefix ^ ".annot" in
  let oc = open_out fn in
  let _  = Pretty.fprint ~width:80 oc d in
  let _  = close_out oc in
  ()

let generate_ispec bs = 
  let fn = !Co.liquidc_file_prefix ^ ".infspec" in
  let oc = open_out fn in
  bs |> Misc.map_partial (function TFun (x,y) -> Some (x,y) | _ -> None)
     |> (fun bs -> PP.seq ~sep:(PP.text "\n\n") ~doit:(fun (fn, cf) ->
         PP.dprintf "%s ::\n@[%a@]" fn Ct.d_refcfun cf) ~elements:bs)
     |> (fun d  -> PP.fprint ~width:80 oc d)
     |> (fun _  -> close_out oc)

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

let d_vartyp () (v, t) = 
  PP.dprintf "(%s :: %a)" v.Cil.vname Cil.d_type v.Cil.vtype

let d_vartypes () vts = 
  PP.seq (PP.text ",") (d_vartyp ()) vts

let d_sloc_vartyps () (sloc, vts) = 
  PP.dprintf "[%a |-> %a]\n" Sloc.d_sloc sloc d_vartypes vts

let d_sloc_typ () (sloc, t) = 
  PP.dprintf "[%a |-> %a]\n" Sloc.d_sloc sloc Cil.d_type t

let d_sloc_typs () slocts = 
  PP.docList ~sep:(PP.dprintf "@!") (d_sloc_typ ()) () slocts

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
  let _ = assertf "deprecated: stitch_shapes_ctypes" in
  Misc.write_to_file (!Constants.liquidc_file_prefix ^ ".shape") "SHAPE INFORMATION";
  SM.iter begin fun fn shp ->
    shp.Shape.vtyps
    >> (fun xs -> shaper := List.rev_append xs !shaper)
    |> Misc.kgroupby (snd <+> Ct.I.CType.sloc)
    |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
    |> List.map (Misc.app_snd (List.map fst))
    |> List.map (Misc.app_snd (Misc.kgroupby (fun v -> v.Cil.vtype)))
    |> PP.docList ~sep:(PP.dprintf "@!") (d_sloc_typ_varss ()) ()
    |> PP.concat (PP.text ("\n\n\nSTITCH SHAPE: "^fn^"\n"))
    |> PP.sprint ~width:80
    |> (Misc.append_to_file (!Constants.liquidc_file_prefix ^ ".shape")) 
  end shm
  (* ; E.log "EXIT: stitch_shapes_ctypes"; exit 0 *)
(**************************************************************************)
(**************************************************************************)
(* {{{

(***** Step 2: Find the Cil-Fields for the indexes of each Ldesc ********)
type cilinfo  = { name : string option; ty   : Cil.typ option }

let d_cilinfo () ci = 
  Pretty.dprintf "%a %a" 
    (Pretty.docOpt (Cil.d_type ())) ci.ty
    (Pretty.docOpt Pretty.text) ci.name 

module CilReft = struct
  type t = Ct.Index.t * FixConstraint.reft * cilinfo 
  let d_refinement () (ix, r, ci) =
    Pretty.dprintf "%a [%a] %a;@!" d_cilinfo ci Ct.Index.d_index ix Ct.d_reft r
  let is_subref    = fun ir1 ir2 -> assert false
  let of_const     = fun c -> assert false
  let top          = Ct.Index.top, Ct.reft_of_top, { name = None; ty = None } 
end

module CilCTypes   = Ct.Make (CilReft)

let fields_of_store (sto : Ct.refldesc SLM.t) (stt : Cil.typ SLM.t) : CilCTypes.LDesc.t SLM.t =
  SLM.mapi begin fun sloc ld -> 
    if SLM.mem sloc stt then 
      decorate_ldesc ld (SLM.find sloc stt)
    else assertf "ERROR: cannot determine ciltyp for" sloc
  end sto

}}} *)

let target_type_of_ptr = function
  | Cil.TPtr (Cil.TFun (_, _, _, _), _) -> 
      assertf "TBD: target_type_of_ptr : function pointer"
  | Cil.TPtr (c, a) ->
      Some (Cil.unrollType c)
  | _ ->
      None

let biggest_type (vs : Cil.varinfo list) : Cil.typ = 
   vs |> Misc.map_partial  (fun v -> target_type_of_ptr v.Cil.vtype)
      |> (function [] -> assertf "biggest type: No pointers!"
                 | ts -> Misc.list_max_with "biggest_type" Cil.bitsSizeOf ts)

let mk_sloc_ciltyp_map (xcts : (Cil.varinfo * Ct.ctype) list) : Cil.typ SLM.t = 
  xcts |> List.filter (snd <+> (function Ct.Ref (_,_) -> true | _ -> false))
       |> Misc.kgroupby (snd <+> Ct.I.CType.sloc) 
       |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
       |> List.map (Misc.app_snd (List.map fst))
       |> List.map (Misc.app_snd biggest_type)
       >> (fun z -> Pretty.printf "SLOCMAP BEGIN:\n%a\nSLOCMAP END.\n" d_sloc_typs z) 
       |> SLM.of_list

(*
let mk_sloc_ciltyp_map binds =
  binds
  |> Misc.map_partial begin function 
       | TVar (n, cr) -> (match FA.varinfo_of_name n with Some v -> Some (v, cr) | _ -> None)
       | _            -> None
     end
  |> ciltyp_of_slocs
*)

(* val unfold_ciltyp : Cil.typ -> Ct.fieldinfo IM.t *)
let unfold_ciltyp = function
  | Cil.TComp (ci, _) -> 
      asserti ci.Cil.cstruct "TBD: unfold_ciltyp: unions";
      ci.Cil.cfields 
      |> List.map (fun fi -> {Ct.fname = Some fi.Cil.fname; Ct.ftype = Some fi.Cil.ftype})
      |> Misc.index_from 0
      |> IM.of_list
  | ty -> IM.single 0 {Ct.fname = None; Ct.ftype = Some ty}


let patch_refldesc slocm sloc ld =  
  if SLM.mem sloc slocm then 
    let ty   = SLM.find sloc slocm in 
    let fldm = ty |> unfold_ciltyp in
    ld |> Misc.flip RCt.LDesc.set_structinfo {Ct.stype = Some ty}
       |> RCt.LDesc.mapn (fun i _ pf -> RCt.Field.set_fieldinfo pf (IM.find i fldm))
  else begin 
    ignore <| Errormsg.warn "patch_ldesc: unknown cil info for %a" Sloc.d_sloc sloc; 
    ld
  end

let patch_refstore slocm sto = 
  RCt.Store.map_ldesc (patch_refldesc slocm) sto

let patch_refcfun slocm cf = 
  RCt.CFun.map_ldesc (patch_refldesc slocm) cf 

let patch_binding slocm = function
  | TSto (x, sto) -> TSto (x, patch_refstore slocm sto)
  | TFun (x, cf ) -> TFun (x, patch_refcfun slocm cf)
  | b -> b (* TODO: patch ctype too *)

let set_cilinfo xcts binds = 
  let _     = assertf "deprecated: set_cilinfo" in
  let slocm = mk_sloc_ciltyp_map xcts in
  Misc.map (patch_binding slocm) binds

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

let apply_solution =
  let s_typ s = RCt.CType.map (Misc.app_snd (FixConstraint.apply_solution s)) in
  let s_fun s = RCt.CFun.map (s_typ s) <+> RCt.CFun.apply_effects (s_typ s) in
  let s_sto s = RCt.Store.map (s_typ s) in
  fun s a -> match a with 
    | TVar (n, cr) -> TVar (n, s_typ s cr)
    | TFun (f, cf) -> TFun (f, s_fun s cf)
    | TSto (f, st) -> TSto (f, s_sto s st) 

let apply_solution s x = 
  Misc.do_catch_ret "Annots.apply_solution" (apply_solution s) x x

(* API *)
let dump_annots so = 
  !annotr 
  (*  |> set_cilinfo !shaper *)
  |> (match so with Some s -> Misc.map (apply_solution s) | _ -> id)
  |> tags_of_binds 
  >> (fst <+> generate_annots)
  >> (snd <+> generate_tags) 
  |> ignore

(* API *)
let dump_infspec decs s =
  let ds = decs 
           |>  Misc.map_partial (function CilMisc.FunDec (fn,_,_) -> Some fn | _ -> None) 
           |>  SS.of_list in
  let bs = !annotr 
           |>  Misc.filter (function TFun (x, y) -> SS.mem x ds  | _ -> false) 
           |>: apply_solution s in
  generate_ispec bs

