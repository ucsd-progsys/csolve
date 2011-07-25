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
module FA = FixAstInterface 
module CF = ConsInfra
module IM = Misc.IntMap
module SM = Misc.StringMap
module SS = Misc.StringSet
module M  = Misc
module P  = Pretty
module CM = CilMisc
module CS = Ctypes.RefCTypes.Spec
module Cs = Constants

open Misc.Ops
open Cil
open ConsVisitor

let mydebug = false

(***************************************************************************)
(*************** Processing SCIs and Globals *******************************)
(***************************************************************************)

let shapem_of_scim cil spec scim vim =
  (SM.empty, SM.empty)
  |> SM.fold begin fun fn (rf, _) (bm, fm) ->
       let cf = Ctypes.cfun_of_refcfun rf in
       if SM.mem fn scim then
         let _ = asserts (SM.mem fn vim) "shapem_of_scim" in
         (bm, (SM.add fn (cf, SM.find fn scim, SM.find fn vim) fm))
       else (SM.add fn cf bm, fm)
     end (CS.funspec spec)
  (* >> (fst <+> Misc.sm_print_keys "builtins") *)
  (* >> (snd <+> Misc.sm_print_keys "non-builtins") *)
  |> snd 
  |> Inferctypes.infer_shapes cil (Ctypes.cspec_of_refspec spec) 

let declared_names decs is_decl =
  decs |> M.map_partial is_decl |> List.fold_left (M.flip SS.add) SS.empty

(* TBD: UGLY *)
let mk_gnv f spec decs cenv =
  let fundecs = declared_names decs (function CM.FunDec (fn,_) -> Some fn | _ -> None) in
  let vardecs = declared_names decs (function CM.VarDec (v, _, _) -> Some v.vname | _ -> None) in
  let gnv0 = spec 
             |> CS.varspec
             |> M.sm_to_list
             |> M.map_partial (fun vs -> if SS.mem (fst vs) vardecs then Some vs else None)
             |> List.map (FA.name_of_string <**> (fst <+> f))
             |> FI.ce_adds FI.ce_empty in
  M.sm_to_list cenv
  |> List.map begin fun (fn, ft) ->
       (fn, if SS.mem fn fundecs 
            then ft |> FI.refcfun_of_cfun |> FI.map_fn f
            else (CS.get_fun fn spec |> fst))
     end
  |> FI.ce_adds_fn gnv0

(********************************************************************************)
(*************************** Unify Spec Names and CIL names *********************)
(********************************************************************************)

let rename_args rf sci = 
  let fn       = sci.ST.fdec.Cil.svar.Cil.vname in
  let xrs      = Ctypes.args_of_refcfun rf in
  let ys       = sci.ST.fdec.Cil.sformals |> List.map (fun v -> v.Cil.vname) in
  let _        = asserts (List.length xrs = List.length ys) "rename_args: bad spec for %s" fn in
  let subs     = Misc.map2 (fun (x,_) y -> Misc.map_pair FA.name_of_string (x,y)) xrs ys in
  let args'    = Misc.map2 (fun (x, rt) y -> (y, FI.t_subs_names subs rt)) xrs ys in
  let ret'     = rf |> Ctypes.ret_of_refcfun |> FI.t_subs_names subs in
  let hi', ho' = rf |> Ctypes.stores_of_refcfun
                    |> Misc.map_pair (FI.refstore_subs FI.t_subs_names subs) in
  Ctypes.RefCTypes.CFun.make args' rf.Ctypes.globlocs hi' ret' ho'

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
  Ctypes.I.Store.Data.fold_locs begin fun l ld sto ->
    try
      let shp_ld = Ctypes.I.Store.Data.find shp_sto l in
        Ctypes.I.Store.Data.add sto l begin
          Ctypes.I.LDesc.mapn begin fun _ pl fld ->
            match Ctypes.I.LDesc.find pl shp_ld with
              | [(_, shp_fld)] -> Ctypes.I.Field.set_finality (Ctypes.I.Field.get_finality shp_fld) fld
              | _              -> fld
          end ld
        end
    with Not_found ->
      Ctypes.I.Store.Data.add sto l ld
  end sto sto

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

(* {{{ 
let print_sccs sccs =
  P.printf "Callgraph sccs:\n\n";
  List.iter (fun fs -> P.printf " [%a]\n" (P.d_list "," (fun () v -> P.text v.Cil.vname)) fs |> ignore) sccs
}}} *)

(* API *)
let create cil spec =
  let reachf = CM.reachable cil in
  let scim   = cil |> scim_of_file |> Misc.sm_filter (fun fn _ -> reachf fn)  in
  let _      = E.log "\nDONE: SSA conversion \n" in
  let tgr    = scim |> Misc.sm_to_list |> Misc.map snd |> CilTag.create in
  let _      = E.log "\nDONE: TAG initialization\n" in
  let spec   = rename_funspec scim spec in
  let _      = E.log "\nDONE: SPEC rename \n" in
  let decs   = decs_of_file cil |> Misc.filter (function CM.FunDec (vn,_) -> reachf vn | _ -> true) in
  let cnv0   = spec |> Ctypes.cspec_of_refspec |> Ctypes.I.Spec.funspec |> SM.map fst in
  let spec0  = Ctypes.I.Spec.map FI.t_true_refctype spec in
  let gnv0   = mk_gnv FI.t_scalar_refctype spec0 decs cnv0 in
  let vim    = BNstats.time "ScalarIndex" (Scalar.scalarinv_of_scim cil spec0 tgr gnv0) scim in
  let shm    = shapem_of_scim cil spec scim vim in
  let _      = Genspec.stitch_shapes_ctypes cil shm in
  let gnv    = cnv0 |> finalize_funtypes shm |> mk_gnv (Ctypes.ctype_of_refctype <+> FI.t_fresh) spec decs in
  let _      = E.log "\nDONE: SHAPE infer \n" in
  let _      = if !Cs.ctypes_only then exit 0 else () in
  let _      = E.log "\nDONE: Gathering Decs \n" in
  let _      = E.log "\nDONE: Global Environment \n" in
  let gst    = spec |> Ctypes.RefCTypes.Spec.store |> Ctypes.store_of_refstore |> FI.refstore_fresh "global" in
  (tgr, cons_of_decs tgr spec gnv gst decs
        |> Consindex.create
        |> cons_of_scis tgr gnv gst scim (Some shm))
