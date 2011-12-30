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

(* This file is part of the CSolve Project.*)
module H   = Hashtbl
module FA  = FixAstInterface
module Ct  = Ctypes
module Co  = Constants
module RCt = Ct.RefCTypes
module PP  = Pretty

module SS  = Misc.StringSet
module SM  = Misc.StringMap
module SLM = Sloc.SlocMap
module IM  = Misc.IntMap
module ST  = Ssa_transform

open Misc.Ops

let mydebug = false


(*******************************************************************)
(****************** Representation for Bindings ********************)
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


(*******************************************************************)
(****************** Gathering Information about Bindings ***********)
(*******************************************************************)

class annotations = object (self)
  val vart         = H.create 37
  val funt         = H.create 37
  val stot         = H.create 37
  val mutable scim = SM.empty
  method add_var   = H.replace vart 
  method add_fun   = H.replace funt
  method add_sto   = H.replace stot

  method get_scim () : ST.t SM.t = scim 

  method set_shape (cil : Cil.file) (shm : Shape.t SM.t) (scim' : ST.t SM.t) : unit =
    scim <- scim';

  
  method get_binds () = 
       List.map (fun (x,y) -> TFun (x, y)) (Misc.hashtbl_to_list funt) 
    ++ List.map (fun (x,y) -> TSto (x, y)) (Misc.hashtbl_to_list stot)
    ++ List.map (fun (x,y) -> TVar (x, y)) (Misc.hashtbl_to_list vart)

  method get_var_type (x: FA.name) : Cil.typ = 
    failwith "TBD"

  method get_fun_dec  (f: string) : Cil.fundec =
    failwith "TBD"

  method get_sloc_type (f: string) (l: Sloc.t) : Cil.typ = 
    failwith "TBD"
end

(*******************************************************************)
(***************** Rendering Annots (Refinements Only) *************)
(*******************************************************************)

let kts_of_bind = function
  | TVar (n, cr) ->
      let x    = FA.string_of_name n in
      [x, ("variable "^x)]
  | TFun (f, cf) -> 
      [f, ("function "^f)]
  | TSto (f, st) -> 
      RCt.Store.domain st 
      |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
      |> List.map (fun s -> (s, s^" |->")) 

let d_bind_orig _ () = function
  | TVar (n, cr) ->
      Pretty.dprintf "variable %s ::\n\n@[%a@] " 
      (FA.string_of_name n) Ct.d_refctype cr
  | TFun (f, cf) -> 
      Pretty.dprintf "function %s ::\n\n@[%a@] " 
      f Ct.d_refcfun cf 
  | TSto (f, st) -> 
      Pretty.dprintf "funstore %s ::\n\n@[%a@] " f Ct.d_refstore st

(*******************************************************************)
(*********************** Rendering (Hybrid) ************************)
(*******************************************************************)

let d_ann_var () ((x: FA.name), (ct: Ct.refctype), (t: Cil.typ)) = 
  failwith "TBD"
let d_ann_fun () ((f: string), (cf: Ct.refcfun), (t: Cil.fundec)) = 
  failwith "TBD"
let d_ann_sto () (lldts : (Sloc.t * Ct.refldesc * Cil.typ) list) = 
  failwith "TBD"

let d_bind_hybrid me () = function 
  | TVar (x, ct) -> 
      d_ann_var () (x, ct, me#get_var_type x) 
  | TFun (f, cf) -> 
      d_ann_fun () (f, cf, me#get_fun_dec f)
  | TSto (f, st) -> 
      Ct.RefCTypes.Store.bindings st
      |> fst    (* ignore funptrs *) 
      |> List.map (fun (l, ld) -> (l, ld, me#get_sloc_type f l))
      |> d_ann_sto ()

(*******************************************************************)
(************************ Write to File ****************************)
(*******************************************************************)

let generate_ispec bs = 
  let fn = !Co.csolve_file_prefix ^ ".infspec" in
  Misc.with_out_file fn begin fun oc -> 
    bs |> Misc.map_partial (function TFun (x,y) -> Some (x,y) | _ -> None)
       |> (fun bs -> PP.seq ~sep:(PP.text "\n\n") ~doit:(fun (fn, cf) ->
             PP.dprintf "%s ::\n@[%a@]" fn Ct.d_refcfun cf) ~elements:bs)
       |> PP.fprint ~width:80 oc
  end

let generate_annots d = 
  Misc.with_out_file (!Co.csolve_file_prefix ^ ".annot") begin fun oc ->
    Pretty.fprint ~width:80 oc d 
  end

let generate_tags kts =
  Misc.with_out_file (!Co.csolve_file_prefix ^ ".tags") begin fun oc -> 
    kts 
    |> List.sort (fun (k1,_) (k2,_) -> compare k1 k2) 
    |> List.iter (fun (k,t) -> ignore <| PP.fprintf oc "%s\t%s.annot\t/%s/\n" k !Co.csolve_file_prefix t) 
  end

let generate_vmap scim =
  Misc.with_out_file (!Co.csolve_file_prefix^".vmap") begin fun oc -> 
    SM.iter begin fun _ sci -> 
      sci.ST.vmapt 
      |> Misc.hashtbl_to_list
      |> Misc.sort_and_compact 
      |> List.iter begin fun ((vname, file, line), ssaname) ->
           let vname = CilMisc.unrename_local sci.ST.fdec.Cil.svar.Cil.vname vname
           in  Printf.fprintf oc "%s \t %s \t %d \t %s \n" vname file line ssaname
         end
    end scim 
  end
 
(*******************************************************************)
(******************************* API *******************************)
(*******************************************************************)

let annr = ref (new annotations)

(* API *)
let annot_shape = (!annr)#set_shape 
let annot_fun   = (!annr)#add_fun
let annot_sto   = (!annr)#add_sto
let annot_var   = (!annr)#add_var
let clear ()    = annr := new annotations 


let d_bind = d_bind_orig (* d_bind_hybrid *) 

(* API *)
let dump_annots so =
  let ann = !annr in
  ann#get_binds () |> Misc.maybe_apply (Misc.map <.> apply_solution) so
                   |> (PP.d_list Co.annotsep_name (d_bind ann) () <*> Misc.flap kts_of_bind)
                   |> (generate_annots <**> generate_tags)
                   |> ignore;
  ann#get_scim ()  |> generate_vmap

(* API *)
let dump_infspec decs s =
  let ds = decs 
           |>  Misc.map_partial (function CilMisc.FunDec (fn,_,_) -> Some fn | _ -> None) 
           |>  SS.of_list in
  let bs = (!annr)#get_binds () 
           |>  Misc.filter (function TFun (x, y) -> SS.mem x ds  | _ -> false) 
           |>: apply_solution s in
  generate_ispec bs

(*******************************************************************)
(*************** Junk from old Cil-Ctype Surgery *******************)
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

 (* YUCK!!! Global State. *)
let shaper    = ref []


(* API *)
let stitch_shapes_ctypes cil shm = 
  let _ = assertf "deprecated: stitch_shapes_ctypes" in
  Misc.write_to_file (!Constants.csolve_file_prefix ^ ".shape") "SHAPE INFORMATION";
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
    |> (Misc.append_to_file (!Constants.csolve_file_prefix ^ ".shape")) 
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


