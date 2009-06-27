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
module E  = Errormsg
module ST = Ssa_transform
module FI = FixInterface 
module CF = Consinfra
module IM = Misc.IntMap
module SM = Misc.StringMap

open Misc.Ops
open Cil

let mydebug = false 

(****************************************************************************)
(***************************** Misc. Helpers ********************************)
(****************************************************************************)

let group_annots xs = 
  List.fold_left begin fun (gs, is, ns) a -> 
    match a with 
    | Refanno.Gen _  -> (a::gs, is, ns)
    | Refanno.Ins _  -> (gs, a::is, ns)
    | Refanno.New _  
    | Refanno.NewC _ -> (gs, is, a::ns)
  end ([], [], []) xs

let lsubs_of_annots ns = 
  List.map (function Refanno.New (x,y)    -> (x,y)
                   | Refanno.NewC (x,_,y) -> (x,y)
                   | _               -> assertf "cons_of_call: bad ns") ns

let extend_world ld binds loc newloc (env, sto) = 
  let subs   = List.map (fun (n,_) -> (n, FI.name_fresh ())) binds in
  let env'   = List.map2 (fun (_, cr) (_, n') -> (n', cr)) binds subs
               |> Misc.map (Misc.app_snd (FI.t_subs_names subs))
               |> FI.ce_adds env in
  let _, im  = List.fold_left (fun (i,im) (_,n') -> (i+1, IM.add i n' im)) (0, IM.empty) subs in
  let ld'    = FI.refldesc_subs ld begin fun i ploc rct ->
                  if IM.mem i im then IM.find i im |> FI.t_name env' else
                    match ploc with 
                    | Ctypes.PLAt _ -> assertf "missing binding!"
                    | _ when newloc -> FI.t_true_refctype rct
                    | _             -> FI.t_subs_names subs rct
               end in
  let sto'   = FI.refstore_set sto loc ld' in
  (env', sto')

let extend_env v cr env =
  FI.ce_adds env [(FI.name_of_varinfo v), cr]

let rename_store lsubs subs sto = 
  sto |> Ctypes.prestore_subs lsubs 
      |> FI.refstore_subs FI.t_subs_exps subs 

let rename_refctype lsubs subs cr =
  cr |> FI.t_subs_locs lsubs
     |> FI.t_subs_exps subs

(****************************************************************************)
(********************** Constraints for Phis ********************************)
(****************************************************************************)

let weaken_undefined me env v = 
  let n = FI.name_of_varinfo v in
  if FI.ce_mem n env && CF.is_undefined me v 
  then FI.ce_rem n env 
  else env

let tcons_of_phis me phia =  
  Misc.array_flapi begin fun i asgns ->
    let envi,_ = CF.outwld_of_block me i in
    let asgns' = Misc.transpose asgns in
    Misc.flap begin fun (j, vvjs) ->
      let pj     = CF.guard_of_block me j (Some i) in
      let locj   = CF.location_of_block me j in
      let envj,_ = CF.outwld_of_block me j in
      let nnjs   = Misc.map (Misc.map_pair FI.name_of_varinfo) vvjs in
      Misc.flap begin fun (v, vj) ->
        let envj  = weaken_undefined me envj v in
        let n, nj = Misc.map_pair FI.name_of_varinfo (v, vj) in
        let lhs   = if not (CF.is_undefined me vj) then FI.t_name envj nj else  
                      FI.ce_find nj envj |> FI.ctype_of_refctype |> FI.t_true in
        let rhs   = FI.ce_find n envi |> FI.t_subs_names nnjs in
        FI.make_cs envj pj lhs rhs locj
      end vvjs 
    end asgns' 
  end phia

let bind_of_phi me v =
  let vn = FI.name_of_varinfo v in
  let cr = CF.ctype_of_varinfo me v |> FI.t_fresh in
  (vn, cr)

let wcons_of_phis me loc env vs =
  let wenv = List.fold_left (weaken_undefined me) env vs in
  Misc.flap begin fun v -> 
    let vn  = FI.name_of_varinfo v in
    let cr  = FI.ce_find vn env in 
    FI.make_wfs wenv cr loc
  end vs

(****************************************************************************)
(********************** Constraints for Annots ******************************)
(****************************************************************************)

let cons_of_annot loc grd (env, sto) = function 
  | Refanno.Gen  (cloc, aloc) -> 
      let sto'   = FI.refstore_remove cloc sto in
      let ld1    = (cloc, FI.refstore_get sto cloc) in
      let ld2    = (aloc, FI.refstore_get sto aloc) in
      let cs     = FI.make_cs_refldesc env grd ld1 ld2 loc in
      ((env, sto'), cs)

  | Refanno.Ins (aloc, cloc) ->
      let _      = asserts (not (FI.refstore_mem cloc sto)) "cons_of_annot: (Ins)!" in
      let aldesc = FI.refstore_get sto aloc in
      let abinds = FI.binds_of_refldesc aloc aldesc in
      let wld    = extend_world aldesc abinds cloc false (env, sto) in
      (wld, [])

  | _ -> assertf "cons_of_annot: New/NewC" 

  
let cons_of_annots me loc grd wld annots =
  Misc.mapfold (cons_of_annot loc grd) wld annots
  |> Misc.app_snd Misc.flatten 

(****************************************************************************)
(********************** Constraints for Assignments *************************)
(****************************************************************************)

let cons_of_set me loc grd (env, sto) = function 
  (* v := *v' *)
  | (Var v, NoOffset), Lval (Mem (Lval (Var v', offset)), _) 
  | (Var v, NoOffset), Lval (Mem (CastE (_, Lval (Var v', offset))), _) ->
      let _  = asserts (offset = NoOffset) "cons_of_set: bad offset1" in
      let cr = FI.ce_find (FI.name_of_varinfo v') env 
               |> FI.refstore_read sto 
               |> FI.t_ctype_refctype (CF.ctype_of_varinfo me v) in
      (extend_env v cr env, sto), []

  (* v := e, where e is pure *)
  | (Var v, NoOffset), e ->
      let _  = CilMisc.check_pure_expr e in
      let cr = FI.t_exp (CF.ctype_of_expr me e) e  
               |> FI.t_ctype_refctype (CF.ctype_of_varinfo me v) in
      (extend_env v cr env, sto), []

  (* *v := e, where e is pure *)
  | (Mem (Lval(Var v, NoOffset)), _), e 
  | (Mem (CastE (_, Lval (Var v, _))), _), e ->
      let addr = FI.ce_find (FI.name_of_varinfo v) env in
      let cr'  = FI.t_exp (CF.ctype_of_expr me e) e in
      if FI.is_soft_ptr sto addr then 
        let cr   = FI.refstore_read sto addr in
        ((env, sto), (FI.make_cs env grd cr' cr loc))
      else
        let sto' = FI.refstore_write sto addr cr' in
        ((env, sto'), [])

  | _ -> assertf "TBD: cons_of_set"

(****************************************************************************)
(********************** Constraints for Calls *******************************)
(****************************************************************************)

let cons_of_call_params me loc grd env lsubs subs es args =
  Misc.flap2 begin fun e (_, cr) ->
    let lhs = FI.t_exp (CF.ctype_of_expr me e) e in
    let rhs = rename_refctype lsubs subs cr in 
    FI.make_cs env grd lhs rhs loc
  end es args

let env_of_retbind lsubs subs env lvo cr = 
  match lvo with 
  | Some ((Var v), NoOffset) -> extend_env v (rename_refctype lsubs subs cr) env
  | None                     -> env
  | _  when !Constants.safe  -> assertf "env_of_retbind"
  | _                        -> env

let poly_clocs_of_store ocst ns = 
  Misc.map_partial begin function 
    | Refanno.NewC (_,a,c) ->
        let _ = asserts (not (Sloc.is_abstract c)) "poly_clocs_of_store" in
        (match FI.binds_of_refldesc c (FI.refstore_get ocst c) with [] -> Some (a,c) | _ -> None)
    | _ -> None
  end ns

let instantiate_cloc me wld (aloc, cloc) = 
  let aldesc = FI.refstore_get (CF.get_astore me) aloc in
  let abinds = FI.binds_of_refldesc aloc aldesc 
               |> List.map (Misc.app_snd FI.t_true_refctype) in
  extend_world aldesc abinds cloc true wld

let cons_of_call me loc grd (env, st) (lvo, fn, es) ns = 
  let _     = Pretty.printf "cons_of_call: fn = %s \n" fn in
  let frt   = FI.ce_find_fn fn env in
  let args  = FI.args_of_refcfun frt |> List.map (Misc.app_fst FI.name_of_string) in
  let lsubs = lsubs_of_annots ns in
  let subs  = asserts (List.length args = List.length es) "cons_of_call: bad params"; 
              List.combine (List.map fst args) es in

  let ist, ost   = FI.stores_of_refcfun frt |> Misc.map_pair (rename_store lsubs subs) in
  let oast, ocst = Ctypes.prestore_split ost in

  let cs1   = cons_of_call_params me loc grd env lsubs subs es args in 
  let cs2   = FI.make_cs_refstore env grd st   ist true  loc in
  let cs3   = FI.make_cs_refstore env grd oast st  false loc in

  let env'  = env_of_retbind lsubs subs env lvo (FI.ret_of_refcfun frt) in
  let st'   = Ctypes.prestore_upd st ocst in
  let wld'  = poly_clocs_of_store ocst ns 
              |> List.fold_left (instantiate_cloc me) (env', st') in
  (wld', cs1 ++ cs2 ++ cs3)

(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let cons_of_annotinstr me loc grd wld (annots, instr) = 
  let gs, is, ns = group_annots annots in
  let wld, anncs = cons_of_annots me loc grd wld (gs ++ is) in
  match instr with 
  | Set (lv, e, _) ->
      let _       = asserts (ns = []) "cons_of_annotinstr: new-in-set" in
      let wld, cs = cons_of_set me loc grd wld (lv, e) in
      (wld, cs ++ anncs)
  | Call (lvo, Lval ((Var fv), NoOffset), es, loc) ->
      let wld, cs = cons_of_call me loc grd wld (lvo, fv.Cil.vname, es) ns in
      (wld, anncs ++ cs)
  | _ -> 
      E.error "cons_of_instr: %a \n" d_instr instr;
      assertf "TBD: cons_of_instr"
 
(****************************************************************************)
(********************** Constraints for [stmt] ******************************)
(****************************************************************************)

let cons_of_ret me loc grd (env, st) e =
  let frt    = FI.ce_find_fn (CF.get_fname me) env in
  let lhs    = FI.t_exp (CF.ctype_of_expr me e) e in 
  let rhs    = FI.ret_of_refcfun frt in
  let _, ost = FI.stores_of_refcfun frt in
  (FI.make_cs env grd lhs rhs loc) ++
  (FI.make_cs_refstore env grd st ost true loc)

let cons_of_annotstmt me loc grd wld (anns, stmt) = 
  match stmt.skind with
  | Instr is ->
      let ann, anns = Misc.list_snoc anns in
      asserts (List.length anns = List.length is) "cons_of_stmt: bad annots instr";
      let wld, cs1 = List.combine anns is 
                     |> Misc.mapfold (cons_of_annotinstr me loc grd) wld in
      let wld, cs2 = cons_of_annots me loc grd wld ann in
      (wld, cs2 ++ Misc.flatten cs1)
  | Return ((Some e), _) ->
      asserts (List.length anns = 0) "cons_of_stmt: bad annots return";
      (wld, cons_of_ret me loc grd wld e)
  | _ ->
      let _ = if !Constants.safe then E.error "unknown annotstmt: %a" d_stmt stmt in
      (wld, [])

(****************************************************************************)
(********************** Constraints for (cfg)block **************************)
(****************************************************************************)

let cons_of_block me i =
  let grd     = CF.guard_of_block me i None in
  let loc     = CF.location_of_block me i in
  let phis    = CF.phis_of_block me i in
  let astmt   = CF.annotstmt_of_block me i in
  let env, st = CF.inwld_of_block me i in
  let env     = List.map (bind_of_phi me) phis |> FI.ce_adds env in
  let ws      = wcons_of_phis me loc env phis in
  let wld, cs = cons_of_annotstmt me loc grd (env, st) astmt in
  (wld, ws, cs)

(****************************************************************************)
(********************** Constraints for ST.ssaCfgInfo ***********************)
(****************************************************************************)

let process_block me i = 
  let wld, ws, cs = cons_of_block me i in
  me |> CF.add_wld i wld 
     |> CF.add_cons ws cs

let process_phis phia me =
  let cs = tcons_of_phis me phia in
  CF.add_cons [] cs me 

let cons_of_sci gnv sci shp =
  let _ = Pretty.printf "%a\n" Refanno.d_block_annotation_array shp.Inferctypes.anna in
  let _ = Pretty.printf "%a\n" Refanno.d_ctab shp.Inferctypes.theta in 
  let _ = Pretty.printf "ICstore = %a\n" Ctypes.d_prestore_addrs shp.Inferctypes.store in
  CF.create gnv sci shp 
  |> Misc.foldn process_block (Array.length sci.ST.phis)
  |> process_phis sci.ST.phis
  |> CF.get_cons

(***************************************************************************)
(*************** Processing SCIs and Globals *******************************)
(***************************************************************************)

let add_scis gnv scim shpm ci = 
  SM.fold begin fun fn sci ci ->
    let _ = Pretty.printf "Constraining %s:\n\n%a\n\n" sci.ST.fdec.svar.vname d_block sci.ST.fdec.sbody in
        cons_of_sci gnv sci (SM.find fn shpm)
     |> Misc.uncurry (Consindex.add ci fn sci)
  end scim ci 

(* NOTE: 1. templates for formals are in "global" gnv, 
         2. each function var is bound to its "output" *) 
let decs_of_file cil = 
  Cil.foldGlobals cil begin fun acc g -> match g with
    | GFun (fdec, loc) -> (fdec.svar.vname, loc) :: acc 
    | _                -> if !Constants.safe then assertf "decs_of_file" else
                          let _ = ignore (E.warn "Ignoring global: %a \n" d_global g) in 
                          acc
  end []

let gnv_of_spec spec gnv = 
  SM.fold begin fun fn ft gnv ->
    if FI.ce_mem_fn fn gnv then gnv else 
      FI.ce_adds_fn gnv [(fn, ft)] 
  end spec gnv

let gnv_of_decs spec decs =
  decs |> List.fold_left begin fun gnv (fn,_) ->
            let fr = Misc.do_catch ("missing spec: "^fn) (SM.find fn) spec in 
            let ft = fr |> FI.cfun_of_refcfun |> FI.t_fresh_fn in 
            FI.ce_adds_fn gnv [(fn, ft)] 
          end FI.ce_empty 
       |> gnv_of_spec spec

let cons_of_decs gnv decs =
  List.fold_left begin fun (ws, cs) (fn, loc) -> 
      ((FI.make_wfs_fn gnv (FI.ce_find_fn fn gnv) loc) ++ ws, cs)
  end ([], []) decs

let scim_of_file cil =
  cil |> ST.scis_of_file 
      |> List.fold_left begin fun acc sci -> 
           let fn = sci.ST.fdec.svar.vname in
           SM.add fn sci acc
         end SM.empty

let shapem_of_scim spec scim =
  (SM.empty, SM.empty)
  |> SM.fold begin fun fn rf (bm, fm) ->
       let cf = FI.cfun_of_refcfun rf in
       if SM.mem fn scim 
       then (bm, (SM.add fn (cf, SM.find fn scim) fm))
       else ((SM.add fn cf bm), fm)
     end spec
  |> (fun (bm, fm) -> Misc.sm_print_keys "builtins" bm; Misc.sm_print_keys "non-builtins" fm; (bm, fm))
  |> (fun (bm, fm) -> Inferctypes.infer_shapes (Misc.sm_extend bm (SM.map fst fm)) fm |> fst)

let rename_args rf sci : FI.refcfun =
  let fn       = sci.ST.fdec.Cil.svar.Cil.vname in
  let xrs      = FI.args_of_refcfun rf in
  let ys       = sci.ST.fdec.Cil.sformals |> List.map (fun v -> v.Cil.vname) in
  let _        = asserts (List.length xrs = List.length ys) "rename_args: bad spec for %s" fn in
  let subs     = List.map2 (fun (x,_) y -> Misc.map_pair FI.name_of_string (x,y)) xrs ys in
  let qls'     = FI.qlocs_of_refcfun rf in
  let args'    = List.map2 (fun (x, rt) y -> (y, FI.t_subs_names subs rt)) xrs ys in
  let ret'     = FI.t_subs_names subs (FI.ret_of_refcfun rf) in
  let hi', ho' = rf |> FI.stores_of_refcfun
                    |> Misc.map_pair (FI.refstore_subs FI.t_subs_names subs) in
  FI.mk_refcfun qls' args' hi' ret' ho' 

let rename_spec scim spec =
  Misc.sm_to_list spec 
  |> List.map begin fun (fn, rf) -> 
      if SM.mem fn scim 
      then (fn, rename_args rf (SM.find fn scim))
      else (fn, rf)
     end
  |> Misc.sm_of_list

(************************************************************************************)
(******************************** API ***********************************************)
(************************************************************************************)

(* API *)
let create cil spec =
  let scim = scim_of_file cil in
  let _    = E.log "DONE: SSA conversion \n" in
  let spec = rename_spec scim spec in
  let shpm = shapem_of_scim spec scim in
  let _    = E.log "DONE: Shape Inference \n" in
  let decs = decs_of_file cil in
  let _    = E.log "DONE: Decls of File \n" in
  let gnv  = gnv_of_decs spec decs in
  let _    = E.log "DONE: Global Environment \n" in
  cons_of_decs gnv decs 
  |> Misc.uncurry Consindex.create
  |> add_scis gnv scim shpm
