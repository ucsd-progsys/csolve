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
module E   = Errormsg

module SS  = Misc.StringSet
module SM  = Misc.StringMap
module NM  = FA.NameMap
module SLM = Sloc.SlocMap
module IM  = Misc.IntMap
module ST  = Ssa_transform

open Misc.Ops

let mydebug = false


(*******************************************************************)
(****************** Representation for Bindings ********************)
(*******************************************************************)

type binding = TVar of FA.name * (Ct.refctype * Cil.typ)
             | TFun of string  * (Ct.refcfun  * Cil.fundec)
             | TSto of string  * Ct.refstore 

let report_bad_binding = function 
  | TVar (x, (cr, _)) ->
      E.warn "\nBad TVar for %s :: \n\n@[%a@]" (FA.string_of_name x) Ct.d_refctype cr
  | TFun (fn, (cf, _)) ->
      E.warn "\nBad TFun for %s ::\n\n@[%a@]" fn Ct.d_refcfun cf
  | TSto (fn, st) -> 
      E.error "\nBad TSto for %s ::\n\n@[%a@]" fn Ct.d_refstore st 

let apply_solution =
  let s_typ s = RCt.CType.map (Misc.app_snd (FixConstraint.apply_solution s)) in
  let s_fun s = RCt.CFun.map (s_typ s) <+> RCt.CFun.apply_effects (s_typ s) in
  let s_sto s = RCt.Store.map (s_typ s) in
  fun s a -> match a with 
    | TVar (n, (cr, z)) -> TVar (n, (s_typ s cr, z))
    | TFun (f, (cf, z)) -> TFun (f, (s_fun s cf, z))
    | TSto (f, st)      -> TSto (f, s_sto s st) 

let apply_solution s x = 
  Misc.do_catch_ret "Annots.apply_solution" (apply_solution s) x x

(*******************************************************************)
(********** Building Map from Fun -> (Sloc -> Cil.typ) *************)
(*******************************************************************)

let target_type_of_ptr = function
  | Cil.TPtr (Cil.TFun _ , _) -> 
  (* | Cil.TPtr (Cil.TFun (_, _, _, _), _) -> *)
      assertf "TBD: target_type_of_ptr : function pointer"
  | Cil.TPtr (c, a) ->
      Some (Cil.unrollType c)
  | _ ->
      None

let biggest_type (vs : Cil.varinfo list) : Cil.typ = 
   vs |> Misc.map_partial  (fun v -> target_type_of_ptr v.Cil.vtype)
      |> (function [] -> assertf "biggest type: No pointers!"
                 | ts -> Misc.list_max_with "biggest_type" Cil.bitsSizeOf ts)
 
let sloc_typem_of_shape sh = 
  sh.Shape.vtyps
  |> List.filter (snd <+> (function Ct.Ref (_,_) -> true | _ -> false))
  |> Misc.kgroupby (snd <+> Ct.I.CType.sloc) 
  |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
  |> List.map (Misc.app_snd (List.map fst <+> biggest_type))
  (* >> (PP.printf "SLOCMAP BEGIN:\n%a\nSLOCMAP END.\n" d_sloc_typs) *)
  |> SLM.of_list

(*******************************************************************)
(*** Decorating refldesc, refstore, refcfun with Cil Information ***)
(*******************************************************************)

let unfold_ciltyp = function
  | Cil.TComp (ci, _) -> 
      asserti ci.Cil.cstruct "TBD: unfold_ciltyp: unions";
      ci.Cil.cfields 
      |> List.map (fun fi -> {Ct.fname = Some fi.Cil.fname; Ct.ftype = Some fi.Cil.ftype})
      |> Misc.index_from 0
      |> IM.of_list
  | ty -> IM.single 0 {Ct.fname = None; Ct.ftype = Some ty}

let decorate_refldesc slocm sloc ld =  
  if SLM.mem sloc slocm then 
    let ty   = SLM.find sloc slocm in 
    let fldm = unfold_ciltyp ty in
    ld |> Misc.flip RCt.LDesc.set_structinfo {Ct.stype = Some ty}
       |> RCt.LDesc.mapn (fun i _ pf -> RCt.Field.set_fieldinfo pf (IM.find i fldm))
  else begin 
    ignore <| Errormsg.warn "Annots.decorate_ldesc: unknown cil info for %a" Sloc.d_sloc sloc; 
    ld
  end

let decorate_refstore slocm sto = 
  RCt.Store.map_ldesc (decorate_refldesc slocm) sto

let decorate_refcfun slocm cf = 
  RCt.CFun.map_ldesc (decorate_refldesc slocm) cf 


(*******************************************************************)
(***************** Rendering Annots (Refinements Only) *************)
(*******************************************************************)

let kts_of_bind = function
  | TVar (n, _) ->
      let x    = FA.string_of_name n in
      [x, ("variable "^x)]
  | TFun (f, _) -> 
      [f, ("function "^f)]
  | TSto (f, st) -> 
      RCt.Store.domain st 
      |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
      |> List.map (fun s -> (s, s^" |->")) 

let d_bind_orig () = function
  | TVar (n, (cr,_)) ->
      Pretty.dprintf "variable %s ::\n\n@[%a@] " 
      (FA.string_of_name n) Ct.d_refctype cr
  | TFun (f, (cf,_)) -> 
      Pretty.dprintf "function %s ::\n\n@[%a@] " 
      f Ct.d_refcfun cf 
  | TSto (f, st) -> 
      Pretty.dprintf "funstore %s ::\n\n@[%a@] " f Ct.d_refstore st

(*******************************************************************)
(*********************** Rendering (Hybrid) ************************)
(*******************************************************************)

let d_ann_fun () ((f: string), (cf: Ct.refcfun), (t: Cil.fundec)) = 
  failwith "TBD:HEREHEREHERE"
let d_ann_sto () ((f: string), (st: Ct.refstore)) = 
  failwith "TBD:HEREHEREHERE"
  (* Ct.RefCTypes.Store.bindings st |> fst    (* ignore funptrs *)  *)

let d_bind_hybrid () = function 
  | TVar (x, (ct, t)) -> 
      let (_,_,ras) as r = Ct.reft_of_refctype ct in
      PP.dprintf "variable %a :: %a %a" 
        FA.d_name x 
        Cil.d_type t
        (CilMisc.d_formatter (FixConstraint.print_ras None)) ras
        (* OR, with VV binder, Ct.d_reft r *)
  | TFun (f, (cf, fundec)) -> 
      d_ann_fun () (f, cf, fundec)
  | TSto (f, st) ->
      d_ann_sto () (f, st)

let d_bind = d_bind_orig (* d_bind_hybrid *) 

(*******************************************************************)
(************************ Write to File ****************************)
(*******************************************************************)

let generate_ispec bs = 
  let fn = !Co.csolve_file_prefix ^ ".infspec" in
  Misc.with_out_file fn begin fun oc -> 
    bs |> Misc.map_partial (function TFun (x,y) -> Some (x,y) | _ -> None)
       |> (fun bs -> PP.seq ~sep:(PP.text "\n\n") ~doit:(fun (fn, (cf, _)) ->
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

let generate_vmap fssam =
  Misc.with_out_file (!Co.csolve_file_prefix^".vmap") begin fun oc -> 
    SM.iter begin fun fn vmap -> 
      vmap
      |> Misc.hashtbl_to_list
      |> Misc.sort_and_compact 
      |> List.iter begin fun ((vname, file, line), ssaname) ->
           let vname = CilMisc.unrename_local fn vname
           in  Printf.fprintf oc "%s \t %s \t %d \t %s \n" vname file line ssaname
         end
    end fssam 
  end

(*******************************************************************)
(************** Hoarding Information About Bindings ****************)
(*******************************************************************)

class annotations = object (self)
  val vart          = H.create 37
  val funt          = H.create 37
  val stot          = H.create 37
  val mutable fssam = (SM.empty : ((string * string * int, string) Hashtbl.t) SM.t)
  val mutable flocm = (SM.empty : (Cil.typ SLM.t) SM.t)
  val mutable fdecm = (SM.empty : Cil.fundec SM.t)

  method private get_binds () = 
       List.map (fun (x,y) -> TFun (x, y)) (Misc.hashtbl_to_list funt) 
    ++ List.map (fun (x,y) -> TSto (x, y)) (Misc.hashtbl_to_list stot)
    ++ List.map (fun (x,y) -> TVar (x, y)) (Misc.hashtbl_to_list vart)

  method private get_var_type (x: FA.name) : Cil.typ = 
    match FA.varinfo_of_name x with 
    | Some v -> v.Cil.vtype 
    | _      -> E.s <| E.error "Annots.get_var_type, unknown name %a\n" FA.d_name x

  method private get_fun_dec  (f: string) : Cil.fundec =
    try SM.find f fdecm with Not_found ->
      E.s <| E.error "Annots.get_fun_dec, unknown function %s\n" f

  method set_shape (shm : Shape.t SM.t) (scim : ST.t SM.t) : unit =
    fssam <- SM.map (fun sci -> sci.ST.vmapt) scim ;
    fdecm <- SM.map (fun sci -> sci.ST.fdec) scim  ;
    flocm <- SM.map (sloc_typem_of_shape) shm

  method add_var x ct = 
    H.replace vart x (ct, self#get_var_type x)
  
  method add_fun f cf = 
    try
      let locm = SM.find f flocm       
      in H.replace funt f (decorate_refcfun locm cf, self#get_fun_dec f)
    with Not_found -> E.s <| E.error "Annots.add_fun, bad function %s \n" f

  method add_sto f st = 
    try
      let locm = SM.find f flocm       
      in H.replace stot f (decorate_refstore locm st)
    with Not_found -> E.s <| E.error "Annots.add_sto, bad function %s \n" f

  method dump_annots so =
    self#get_binds () 
    |> Misc.maybe_apply (Misc.map <.> apply_solution) so
    |> (PP.d_list Co.annotsep_name d_bind () <*> Misc.flap kts_of_bind)
    |> (generate_annots <**> generate_tags)
    |> (fun _ -> generate_vmap fssam)

  method dump_infspec decs s =
    let ds = decs |> Misc.map_partial (function CilMisc.FunDec (fn,_,_) -> Some fn | _ -> None) |>  SS.of_list in
    Misc.hashtbl_to_list funt
    |> Misc.filter (fst <+> Misc.flip SS.mem ds)
    |> Misc.map (fun (x,y) -> apply_solution s (TFun (x,y)))
    |> generate_ispec
end


(*******************************************************************)
(******************************* API *******************************)
(*******************************************************************)

let annr = ref (new annotations)

(* API *)
let annot_shape  = (!annr)#set_shape 
let annot_fun    = (!annr)#add_fun
let annot_sto    = (!annr)#add_sto
let annot_var    = (!annr)#add_var
let clear ()     = annr := new annotations 
let dump_annots  = fun so -> (!annr)#dump_annots so
let dump_infspec = fun decs s -> (!annr)#dump_infspec decs s


(* {{{ Junk from old Cil-Ctype Surgery

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



