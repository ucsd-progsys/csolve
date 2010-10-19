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

module E  = Errormsg
module ST = Ssa_transform
module FI = FixInterface 
module CF = ConsInfra2
module IM = Misc.IntMap
module SM = Misc.StringMap
module SS = Misc.StringSet
module M  = Misc
module P  = Pretty
module CM = CilMisc
module CS = FI.RefCTypes.Spec
module Cs = Constants

open Misc.Ops
open Cil
open ConsVisitor

let mydebug = false

(***************************************************************************)
(*************** Processing SCIs and Globals *******************************)
(***************************************************************************)

(* PMRM: 

let infer_shapes cil spec scim =
  let spec = FI.cspec_of_refspec spec in
  (Inferctypes.infer_shapes cil spec scim, spec |> Ctypes.I.Spec.funspec |> SM.map fst)
*)

let shapem_of_scim cil spec scim =
  (SM.empty, SM.empty)
  |> SM.fold begin fun fn (rf, _) (bm, fm) ->
       let cf = FI.cfun_of_refcfun rf in
       if SM.mem fn scim
       then (bm, (SM.add fn (cf, SM.find fn scim) fm))
       else (SM.add fn cf bm, fm)
     end (CS.funspec spec)
  >> (fst <+> Misc.sm_print_keys "builtins")
  >> (snd <+> Misc.sm_print_keys "non-builtins")
  |> snd 
  (* PMRM: |> infer_shapes cil spec *)
  |> Inferctypes.infer_shapes cil (FI.cspec_of_refspec spec)

(* TBD: UGLY *)
let mk_gnv spec decs cenv =
  let decs = decs 
             |> Misc.map_partial (function CM.FunDec (fn,_) -> Some fn | _ -> None)
             |> List.fold_left (Misc.flip SS.add) SS.empty in
  let gnv0 = spec 
             |> CS.varspec
             |> M.sm_to_list
             |> List.map begin fun (vn, (vty, _)) -> 
                 (FI.name_of_string vn, vty |> FI.ctype_of_refctype |> FI.t_fresh) 
                end
             |> FI.ce_adds FI.ce_empty in
  M.sm_to_list cenv
  |> List.map begin fun (fn, ft) ->
       (fn, if SS.mem fn decs then FI.t_fresh_fn ft else (CS.get_fun fn spec |> fst))
     end
  |> FI.ce_adds_fn gnv0

(********************************************************************************)
(*************************** Unify Spec Names and CIL names *********************)
(********************************************************************************)

let rename_args rf sci : FI.refcfun =
  let fn       = sci.ST.fdec.Cil.svar.Cil.vname in
  let xrs      = FI.args_of_refcfun rf in
  let ys       = sci.ST.fdec.Cil.sformals |> List.map (fun v -> v.Cil.vname) in
  let _        = asserts (List.length xrs = List.length ys) "rename_args: bad spec for %s" fn in
  let subs     = Misc.map2 (fun (x,_) y -> Misc.map_pair FI.name_of_string (x,y)) xrs ys in
  let qls'     = FI.qlocs_of_refcfun rf in
  let args'    = Misc.map2 (fun (x, rt) y -> (y, FI.t_subs_names subs rt)) xrs ys in
  let ret'     = FI.t_subs_names subs (FI.ret_of_refcfun rf) in
  let hi', ho' = rf |> FI.stores_of_refcfun
                    |> Misc.map_pair (FI.refstore_subs FI.t_subs_names subs) in
  FI.mk_refcfun qls' args' hi' ret' ho' 

let rename_funspec scim spec = 
  spec 
  |> CS.funspec
  |> SM.mapi begin fun fn (rf,b) -> 
       if SM.mem fn scim
       then (rename_args rf (SM.find fn scim), b)
       else (rf, b)
     end
  |> (fun x -> CS.make x (CS.varspec spec) (CS.store spec))

(******************************************************************************)
(********** Strengthen Final Fields in Fun Types from Inferred Shapes *********)
(******************************************************************************)

let finalize_store shp_sto sto =
  Sloc.SlocMap.mapi begin fun l ld ->
    try
      let shp_ld = Sloc.SlocMap.find l shp_sto in
        Ctypes.I.LDesc.mapn begin fun _ pl fld ->
          match Ctypes.I.LDesc.find pl shp_ld with
            | [(_, shp_fld)] -> Ctypes.I.Field.set_finality (Ctypes.I.Field.get_finality shp_fld) fld
            | _              -> fld
        end ld
    with Not_found ->
      ld
  end sto

let finalize_funtypes shm cnv =
  SM.fold begin fun fname shp cnv ->
    let cf = SM.find fname cnv in
      SM.add
        fname
        {cf with
           Ctypes.sto_in  = finalize_store shp.Shape.store cf.Ctypes.sto_in;
           Ctypes.sto_out = finalize_store shp.Shape.store cf.Ctypes.sto_out}
        cnv
  end shm cnv

let tag_of_global = function
  | GType (_,_)    -> "GType"
  | GCompTag (_,_) -> "GCompTag"
  | _              -> "Global"

let decs_of_file cil = 
  Cil.foldGlobals cil begin fun acc g -> match g with
    | GFun (fdec, loc)                  -> CM.FunDec (fdec.svar.vname, loc) :: acc
    | GVar (v, ii, loc) 
      when not (isFunctionType v.vtype) -> CM.VarDec (v, loc, ii.init) :: acc
    | GVarDecl (v, loc) 
      when not (isFunctionType v.vtype) -> CM.VarDec (v, loc, None) :: acc
    | GVarDecl (v, _)
      when (isFunctionType v.vtype)     -> acc
    | GType _ | GCompTag _
    | GCompTagDecl _| GText _
    | GPragma _                         -> acc
    | _ when !Cs.safe                   -> assertf "decs_of_file"
    | _                                 -> E.warn "Ignoring %s: %a \n" (tag_of_global g) d_global g 
                                           |> fun _ -> acc
  end []

let scim_of_file cil =
  ST.scis_of_file cil
  |> List.fold_left begin fun acc sci -> 
       SM.add sci.ST.fdec.svar.vname sci acc
     end SM.empty

(*
let print_sccs sccs =
  P.printf "Callgraph sccs:\n\n";
  List.iter (fun fs -> P.printf " [%a]\n" (P.d_list "," (fun () v -> P.text v.Cil.vname)) fs |> ignore) sccs
*)

(* API *)
let create cil (spec: FI.refspec) =
  let reachf = CM.reachable cil in
  let scim   = cil |> scim_of_file |> Misc.sm_filter (fun fn _ -> reachf fn)  in
  let _      = E.log "\nDONE: SSA conversion \n" in
  let tgr    = scim |> Misc.sm_to_list |> Misc.map snd |> CilTag.create in
  let _      = E.log "\nDONE: TAG initialization\n" in
  let spec   = rename_funspec scim spec in
  let _      = E.log "\nDONE: SPEC rename \n" in
  let decs   = decs_of_file cil |> Misc.filter (function CM.FunDec (vn,_) -> reachf vn | _ -> true) in
  let cnv0   = spec |> FI.cspec_of_refspec |> Ctypes.I.Spec.funspec |> SM.map fst in
  let gnv0   = mk_gnv spec decs cnv0 in
  (* RJ: scalar.* will be hoisted here after it is done, should not depend on shm *)
  let shm    = shapem_of_scim cil spec scim in
  let gnv    = cnv0 |> finalize_funtypes shm |> mk_gnv spec decs in
    
  let _      = if !Cs.scalar then Scalar.test cil spec tgr gnv0 scim shm in
  let _      = E.log "\nDONE: SHAPE infer \n" in
  let _      = if !Cs.ctypes_only then exit 0 else () in
  let _      = E.log "\nDONE: Gathering Decs \n" in
  let _      = E.log "\nDONE: Global Environment \n" in
  let gst    = spec |> FI.RefCTypes.Spec.store |> FI.store_of_refstore |> FI.refstore_fresh "global" in
  (tgr, cons_of_decs tgr spec gnv gst decs
        |> Consindex.create
        |> cons_of_scis tgr gnv gst scim (Some shm))
