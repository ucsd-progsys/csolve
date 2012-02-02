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
module CM  = CilMisc

open Misc.Ops

let mydebug = false

(*******************************************************************)
(****************** Random Printers for Debugging ******************)
(*******************************************************************)

let d_vartyp () (v, t) = 
  PP.dprintf "(%s [%a] :: %a)" v.Cil.vname Ct.I.CType.d_ctype t Cil.d_type v.Cil.vtype

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

let target_type_of_ptr t = match Cil.unrollType t with 
(*  | Cil.TPtr (Cil.TFun _ , _) -> 
      assertf "TBD: target_type_of_ptr : function pointer"
*)  | Cil.TPtr (t, _) | Cil.TArray (t,_,_) ->
      Some (Cil.unrollType t)
    | _ ->
      None

let biggest_type (vs : Cil.varinfo list) : Cil.typ = 
   vs |> Misc.map_partial (fun v -> target_type_of_ptr v.Cil.vtype)
      |> (function [] -> E.s <| E.error "Annots.biggest type: No pointers! %a"
                         (PP.d_list ", " (CM.d_var)) vs
                 | ts -> Misc.list_max_with "biggest_type" Cil.bitsSizeOf ts)
 
let sloc_typem_of_shape sh =
  sh.Shape.vtyps
  >> wwhen mydebug (E.log "Annots.sloc_typem_of_shape: %a \n" d_vartypes)
  |> List.filter (snd <+> (function Ct.Ref (_,_) -> true | _ -> false))
  |> Misc.kgroupby (snd <+> Ct.I.CType.sloc) 
  |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
  |> List.map (Misc.app_snd (List.map fst <+> biggest_type))
  (* >> (PP.printf "SLOCMAP BEGIN:\n%a\nSLOCMAP END.\n" d_sloc_typs) *)
  |> SLM.of_list

(*******************************************************************)
(*** Decorating refldesc, refstore, refcfun with Cil Information ***)
(*******************************************************************)

  (*
let unfold_ciltyp = function
  | Cil.TComp (ci, _) -> 
      asserti ci.Cil.cstruct "TBD: unfold_ciltyp: unions";
      ci.Cil.cfields 
      |> List.map (fun fi -> {Ct.fname = Some fi.Cil.fname; Ct.ftype = Some fi.Cil.ftype})
      |> Misc.index_from 0
      |> IM.of_list
  | ty -> IM.single 0 {Ct.fname = None; Ct.ftype = Some ty}

let d_ciltypm () m =
  m |> IM.to_list 
    |> CM.d_many_braces true (CM.d_pair (fun _ -> Pretty.num) Ct.d_fieldinfo) () 

let unfold_ciltyp x = 
  x |> unfold_ciltyp
    >> (E.log "Annots.unfold_ciltyp %a@!%a@!" Cil.d_type x d_ciltypm) 
*)

let fieldinfo_of_cilfield prefix fi = 
  { Ct.fname = Some (prefix ^ fi.Cil.fname); Ct.ftype = Some fi.Cil.ftype }

let rec unfold_compinfo prefix ci = 
  let _  = asserti ci.Cil.cstruct "TBD: unfold_compinfo: unions" in
  ci.Cil.cfields 
  |> Misc.flap (unfold_fieldinfo prefix)

and unfold_fieldinfo prefix fi = 
  match Cil.unrollType fi.Cil.ftype with
  | Cil.TComp (ci, _) ->
      unfold_compinfo (prefix ^  fi.Cil.fname ^ ".") ci
  | _ ->
      [fieldinfo_of_cilfield prefix fi]


let unfold_compinfo pfx ci = 
  unfold_compinfo pfx ci 
  >> wwhen mydebug (E.log "unfold_compinfo: pfx = <%s> result = %a\n"  pfx (CM.d_many_braces false Ct.d_fieldinfo))

let unfold_ciltyp = function 
  | Cil.TComp (ci,_) ->
      unfold_compinfo "" ci
      |> Misc.index_from 0 
      |> IM.of_list 
      |> (fun im _ i -> IM.find i im)
  | Cil.TArray (t',_,_) ->
      (fun _ i -> { Ct.fname = None ; Ct.ftype = Some t'})
  | t -> 
      (fun _ i -> { Ct.fname = None; Ct.ftype = Some t}) 

let decorate_refldesc slocm sloc ld =  
  if SLM.mem sloc slocm then 
    let ty   = SLM.find sloc slocm in 
    let fldm = unfold_ciltyp ty in
    ld |> Misc.flip RCt.LDesc.set_structinfo {Ct.stype = Some ty}
       |> RCt.LDesc.mapn begin fun i _ pf -> 
            try RCt.Field.set_fieldinfo pf (fldm ld i) with Not_found -> 
              let _ = E.warn "WARNING: Annots.decorate_refldesc: %a bad idx %d, ld=%a, t=%a \n" 
                              Sloc.d_sloc sloc i RCt.LDesc.d_ldesc ld Cil.d_type ty
              in pf
          end
  else begin 
    ignore <| Errormsg.log "WARNING: Annots.decorate_ldesc: unknown cil info for %a \n" Sloc.d_sloc sloc; 
    ld
  end

let has_fldtype fld = 
  Misc.maybe_bool (RCt.Field.get_fieldinfo fld).Ct.ftype

let check_ld_bindings slocm l ld =
  let iflds = RCt.LDesc.bindings ld in
  if not (List.for_all (snd <+> has_fldtype) iflds) then
    if not (SLM.mem l slocm) then 
      E.warn "Annots.check_ld_bindings unknown sloc %a \n" Sloc.d_sloc l
    else
      E.warn "Annots.check_ld_bindings bad fields for %a |-> %a" 
      Sloc.d_sloc l RCt.LDesc.d_ldesc ld

let decorate_refldesc slocm l ld = 
  decorate_refldesc slocm l ld 
  >> check_ld_bindings slocm l 

let decorate_refstore slocm sto = 
  RCt.Store.map_ldesc (decorate_refldesc slocm) sto

let decorate_refcfun slocm f cf = 
  try
    RCt.CFun.map_ldesc (decorate_refldesc slocm) cf 
  with _ -> E.s <| E.error "Annots.decorate_refcfun %s" f

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


let d_ann_ref () = function
  | Ct.Ref (l,_) -> PP.dprintf "REF(%a)" Sloc.d_sloc l
  | _            -> PP.nil

type binder = N of FA.name | S of string | I of Index.t | Nil

let d_binder () = function 
  | N n -> FA.d_name () n
  | S s -> Pretty.text s
  | I i -> Pretty.dprintf "@@%a" Index.d_index i
  | Nil -> Pretty.nil

let d_ann_var () (b, ct, t) =
  let (_,_,ras) as r = Ct.reft_of_refctype ct in
  PP.dprintf "%a %a %a %a" 
    CM.d_type_noattrs t
    d_ann_ref ct
    d_binder b 
    (CM.d_formatter (FixConstraint.print_ras None)) ras
    (* OR, with VV binder, Ct.d_reft r *)

let d_ann_field () (i, fld) = 
  let ct = RCt.Field.type_of fld in
  match RCt.Field.get_fieldinfo fld with
  | { Ct.fname = Some fldname; Ct.ftype = Some t } ->
      d_ann_var () (S fldname, RCt.Field.type_of fld, t )
  | { Ct.ftype = Some t } ->
      d_ann_var () (I i, RCt.Field.type_of fld, t)
  | _ -> 
      PP.dprintf "%a ??? %a" d_binder (I i) d_ann_ref (RCt.Field.type_of fld)

let d_structinfo () = function
  | {Ct.stype = Some t } -> CM.d_type_noattrs () t
  | _                    -> PP.nil


let d_ann_refldesc () ((l : Sloc.t), (ld: Ct.refldesc)) =
  RCt.LDesc.bindings ld
  |> PP.dprintf "%a %a |-> %a" 
        d_structinfo (RCt.LDesc.get_structinfo ld)
        Sloc.d_sloc l (CM.d_many_braces true d_ann_field) 

let stitch_args fn cf = function 
  | None -> 
      E.s <| E.error "Annots.stitch_args no args for %s" fn
  | Some yts -> 
      let m = SM.of_list <| List.map (fun (y,t,_) -> (y,t)) yts in
      Misc.map_partial begin fun (x, ct) ->
        try  Some (S x, ct, SM.find x m) 
        with Not_found -> None 
      end cf.Ct.args

let d_ann_fun () (f, cf, fd) =
  let ret, argso, _, _ = Cil.splitFunctionTypeVI fd.Cil.svar in
  let xoctts           = stitch_args f cf argso              in
  CM.concat_docs 
    [ PP.dprintf "function %s ::@!@!" f
    ; PP.dprintf "%a@!"     d_ann_var (Nil, cf.Ct.ret, ret)
    ; PP.dprintf "%s %a @!" f (CM.d_many_parens true d_ann_var) xoctts 
    (* ; effects *)
    ]
  
let d_ann_stores () ((f: string), (stos: (Ct.refstore*Ct.varstore) list)) =
  stos 
  |> Misc.flap (fst <+> Ct.RefCTypes.Store.bindings)
  |> Misc.kgroupby (fst <+> Sloc.to_string)
  |> Misc.flap snd
  |> PP.dprintf "funstore %s ::@!@!%a" f (PP.d_list "\n\n" d_ann_refldesc)

let d_bind_hybrid () = function 
  | TVar (x, (ct, t)) -> 
      PP.dprintf "variable %a ::@!@!@[%a@]@!@!" 
        FA.d_name x d_ann_var (N x, ct, t)
  | TFun (f, (cf, fundec)) ->
      CM.concat_docs 
        [ d_ann_fun () (f, cf, fundec) 
        ; PP.text Co.annotsep_name
        ; d_ann_stores () (f, [cf.Ct.sto_in; cf.Ct.sto_out])]
  | TSto (f, st) ->
      PP.nil (* d_ann_stores () (f, [st]) *)

let d_bind = (* d_bind_orig *) d_bind_hybrid

let d_bind_raw () = function
  | TVar (x, _) -> PP.dprintf "variable %a" FA.d_name x 
  | TFun (f, _) -> PP.dprintf "function %s" f
  | TSto (f, _) -> PP.dprintf "funstore %s" f



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
           let vname = CM.unrename_local fn vname
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

  method private get_binds () : binding list = 
    (   (List.map (fun (x,y) -> TFun (x, y)) (Misc.hashtbl_to_list funt))
     ++ (List.map (fun (x,y) -> TSto (x, y)) (Misc.hashtbl_to_list stot))
     ++ (List.map (fun (x,y) -> TVar (x, y)) (Misc.hashtbl_to_list vart))
    ) >> wwhen mydebug (List.length <+> E.log "\n\nAnnots.dump_annots (%d)\n\n" (*PP.d_list "\n" d_bind_raw*))


  method private get_flocm (f: string) : (Cil.typ SLM.t) option =
    try Some (SM.find f flocm) with Not_found ->     
      let _ = E.log "Annots: Missing Location-Types for %s \n" f 
      in None

  method private get_fun_dec  (f: string) : Cil.fundec =
    try SM.find f fdecm with Not_found ->
      E.s <| E.error "Annots.get_fun_dec, unknown function %s\n" f

  method set_shape (shm : Shape.t SM.t) (scim : ST.t SM.t) (cfm : Ct.refcfun SM.t) : unit =
    fssam <- SM.map (fun sci -> sci.ST.vmapt) scim ;
    fdecm <- SM.map (fun sci -> sci.ST.fdec) scim  ;
    flocm <- SM.map (sloc_typem_of_shape) shm;
    SM.iter self#add_fun cfm

  method add_var x ct =
    Misc.maybe_iter begin fun v ->
      (* E.log "Annots.add_var %a \n" FA.d_name x; *)
      H.replace vart x (ct, v.Cil.vtype);
    end (FA.varinfo_of_name x)

  method private add_fun f cf = 
    Misc.maybe_iter begin fun locm ->
      H.replace funt f (decorate_refcfun locm f cf, self#get_fun_dec f)
    end (self#get_flocm f)

  method add_sto f st = 
    Misc.maybe_iter begin fun locm ->
      H.replace stot f (decorate_refstore locm st)
    end (self#get_flocm f)

  method dump_annots so =
    self#get_binds () 
    |> Misc.maybe_apply (Misc.map <.> apply_solution) so
    |> (PP.d_list Co.annotsep_name d_bind () <*> Misc.flap kts_of_bind)
    |> (generate_annots <**> generate_tags)
    |> (fun _ -> generate_vmap fssam)

  method dump_infspec decs s =
    let ds = decs |> Misc.map_partial (function CM.FunDec (fn,_,_) -> Some fn | _ -> None) |>  SS.of_list in
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
let annot_shape  = fun x y z  -> (!annr)#set_shape x y z 
let annot_sto    = fun x y    -> (!annr)#add_sto x y
let annot_var    = fun x y    -> (!annr)#add_var x y
let clear        = fun _      -> annr := new annotations 
let dump_annots  = fun so     -> (!annr)#dump_annots so
let dump_infspec = fun decs s -> (!annr)#dump_infspec decs s

(* {{{ Junk from old Cil-Ctype Surgery

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



