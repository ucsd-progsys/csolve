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
module FA = FixAstInterface 
module FI = FixInterface 
module CF = ConsInfra
module IM = Misc.IntMap
module SM = Misc.StringMap
module SS = Misc.StringSet
module M  = Misc
module P  = Pretty
module CM = CilMisc
module Ct = Ctypes
module CS = Ct.RefCTypes.Spec
module RS = Ct.RefCTypes.Store
module Cs = Constants
module Sh = Shape

open Misc.Ops
open Cil

let mydebug = false 

(****************************************************************************)
(***************************** Misc. Helpers ********************************)
(****************************************************************************)

let group_annots xs = 
  List.fold_left begin fun (gs, is, ns) a -> 
    match a with 
    | Refanno.WGen _  
    | Refanno.Gen _  -> (a::gs, is, ns)
    | Refanno.Ins _  -> (gs, a::is, ns)
    | Refanno.New _  
    | Refanno.NewC _ -> (gs, is, a::ns)
  end ([], [], []) xs

let lsubs_of_annots ns = 
  List.map (function Refanno.New (x,y)    -> (x, y)
                   | Refanno.NewC (x,_,y) -> (x, y)
                   | _               -> assertf "cons_of_call: bad ns") ns

let d_lsub () (x,y) = 
  Pretty.dprintf "(%a, %a)" Sloc.d_sloc x Sloc.d_sloc y 

let d_lsubs () xys =
  Pretty.seq (Pretty.text ",") (d_lsub ()) xys

let rename_store lsubs subs sto =
  sto |> FI.refstore_subs_locs lsubs 
      |> FI.refstore_subs FI.t_subs_exps subs 
      |> RS.subs lsubs

(*
let rename_store lsubs subs st = 
  st |> Ct.prestore_subs lsubs
     |> Ct.prestore_map_ct (rename_refctype lsubs subs)
*)

let weaken_undefined me rm env v = 
  let n = FA.name_of_varinfo v in
  let b = FI.ce_mem n env && CF.is_undefined me v in
  if not b then env else
    if rm then FI.ce_rem n env else
      let r    = FI.ce_find n env |> FI.t_true_refctype in
      let env' = FI.ce_rem n env in
      FI.ce_adds env' [(n,r)]

(****************************************************************************)
(********************** Constraints for Annots ******************************)
(****************************************************************************)

let strengthen_instantiated_aloc ffm ptrname aloc ld =
  Ct.RefCTypes.LDesc.mapn (fun _ pl fld -> FI.strengthen_final_field (Sloc.SlocMap.find aloc ffm) ptrname pl fld) ld

let cons_of_annot me loc tag grd ffm (env, sto, tago) = function 
  | Refanno.Gen  (cloc, aloc) ->
      let _      = CM.assertLoc loc (RS.mem sto cloc) "cons_of_annot: (Gen)!" in
      let sto'   = RS.remove sto cloc in
      let ld1    = (cloc, Ct.refstore_get sto cloc) in
      let ld2    = (aloc, Ct.refstore_get sto aloc) in
      let cds    = FI.make_cs_refldesc env grd ld1 ld2 tago tag loc in
      ((env, sto', tago), cds)

  | Refanno.WGen  (cloc, aloc) ->
      let _      = CM.assertLoc loc (RS.mem sto cloc) "cons_of_annot: (WGen)!" in
      let sto'   = RS.remove sto cloc in
      ((env, sto', tago), ([],[]))

  | Refanno.Ins (ptr, aloc, cloc) ->
      let _          = CM.assertLoc loc (not (RS.mem sto cloc)) "cons_of_annot: (Ins)!" in
      let strengthen = strengthen_instantiated_aloc ffm ptr aloc in
      let wld',_     = FI.extend_world sto aloc cloc false strengthen loc tag (env, sto, tago) in
      (wld', ([], []))

  | _ -> assertf "cons_of_annot: New/NewC" 

let cons_of_annots me loc tag grd wld ffm annots =
  Misc.mapfold (cons_of_annot me loc tag grd ffm) wld annots
  |> Misc.app_snd Misc.splitflatten 

(****************************************************************************)
(********************** Constraints for Assignments *************************)
(****************************************************************************)

let extend_env me v cr env = 
  let ct = CF.ctype_of_varinfo me v in
  let cr = FI.t_ctype_refctype ct cr in
(*  let _  = Pretty.printf "extend_env: %s :: %a \n" v.Cil.vname Ct.d_refctype cr in
*)  FI.ce_adds env [(FA.name_of_varinfo v), cr]

let cons_of_mem me loc tago tag grd env v =
  if !Cs.manual then
    ([], [])
  else
    let rct = v |> FA.name_of_varinfo |> FI.t_name env in
    FI.make_cs_validptr env grd rct tago tag loc

let cons_of_rval me loc tag grd (env, sto, tago) = function
  (* *v *)
  | Lval (Mem (Lval (Var v', NoOffset)), _)
  | Lval (Mem (CastE (_, Lval (Var v', NoOffset))), _) ->
      (FI.ce_find (FA.name_of_varinfo v') env |> Ct.refstore_read loc sto,
      cons_of_mem me loc tago tag grd env v')
  (* x, when x is global *)
  | Lval (Var v, NoOffset) when v.vglob ->
      (FI.ce_find (FA.name_of_varinfo v) env, ([], []))
  | AddrOf (Var v, NoOffset) as e when CM.is_fun v ->
      begin match FI.t_exp env (CF.ctype_of_expr me e) e with
        | Ct.Ref (l, _) as rct -> (rct, FI.make_cs_refcfun env grd (FI.ce_find_fn v.vname env) (RS.Function.find sto l) tag loc)
        | _                    -> assert false
      end
  (* e, where e is pure *)
  | e when CM.is_pure_expr e ->
      (FI.t_exp env (CF.ctype_of_expr me e) e, ([], []))
  | e -> 
      E.s <| errorLoc loc "cons_of_rval: impure expr: %a" Cil.d_exp e 

let cons_of_set me loc tag grd ffm (env, sto, tago) = function
  (* v := e, where v is local *)
  | (Var v, NoOffset), rv when not v.Cil.vglob ->
      let cr, cds = cons_of_rval me loc tag grd (env, sto, tago) rv in
      (extend_env me v cr env, sto, Some tag), cds

  (* v := e, where v is global *)
  | (Var v, NoOffset), rv when v.Cil.vglob ->
      let cr, (cs1, _) = cons_of_rval me loc tag grd (env, sto, tago) rv in
      let cs2, _       = FI.make_cs env grd cr (CF.refctype_of_global me v) tago tag loc in
      (env, sto, Some tag), (cs1 ++ cs2, [])

  (* *v := e, where e is pure *)
  | (Mem (Lval(Var v, NoOffset)), _), e 
  | (Mem (CastE (_, Lval (Var v, NoOffset))), _), e ->
      let addr = if v.vglob then CF.refctype_of_global me v else FI.ce_find (FA.name_of_varinfo v) env in
      let cr'  = FI.t_exp env (CF.ctype_of_expr me e) e in
      let cs1, ds1 = cons_of_mem me loc tago tag grd env v in
      let isp  = try Ct.is_soft_ptr loc sto addr with ex ->
                   Errormsg.s <| Cil.errorLoc loc "is_soft_ptr crashes on %s" v.vname in
      if isp then
        let cr       = Ct.refstore_read loc sto addr in
        let cs2, ds2 = FI.make_cs env grd cr' cr tago tag loc in
        (env, sto, Some tag), (cs1 ++ cs2, ds1 ++ ds2)
      else
        let sto      = Ct.refstore_write loc sto addr cr' in
        let env, sto = FI.refstore_strengthen_addr loc env sto ffm v.vname addr in
        (env, sto, Some tag), (cs1, ds1)

  | _ -> assertf "TBD: cons_of_set"

let cons_of_set me loc tag grd ffm (env, sto, tago) ((lv, e) as x) = 
  Misc.do_catchu (cons_of_set me loc tag grd ffm (env, sto, tago)) x 
    (fun ex -> E.error "(%s) cons_of_set [%a] : %a := %a \n" 
               (Printexc.to_string ex) d_loc loc d_lval lv d_exp e)

(****************************************************************************)
(********************** Constraints for Calls *******************************)
(****************************************************************************)

let env_of_retbind me loc grd tag lsubs subs env sto lvo cr = 
  match lvo with 
  | Some ((Var v), NoOffset) ->
      let rct = FI.rename_refctype lsubs subs cr in
        if FI.may_contain_deref rct then
          let frct   = rct |> Ct.ctype_of_refctype |> FI.t_fresh in
          let cs, ds = FI.make_cs env grd rct frct None tag loc in
          let ws     = FI.make_wfs env sto frct tag in
            (extend_env me v frct env, cs, ds, ws)
        else
          (extend_env me v rct env, [], [], [])
  | None              -> (env, [], [], [])
  | _  when !Cs.safe  -> assertf "env_of_retbind"
  | _                 -> (env, [], [], [])

let instantiate_poly_clocs me env grd loc tag' ((_, st',_) as wld) ns =
  let asto = CF.get_astore me in
  ns |> Misc.map_partial (function Refanno.NewC (_,al,cl) -> Some (al,cl) | _ -> None)
     |> List.filter (snd <+> FI.is_poly_cloc st')
     |> Misc.mapfold (fun wld (al, cl) -> FI.extend_world asto al cl true id loc tag' wld) wld
     |> Misc.app_snd List.flatten

let bindings_of_call loc args es =
  let _ = if (List.length args <> List.length es) then (E.s <| errorLoc loc "binds_of_call: bad params") in
  Misc.map2 (fun (n, t) e -> Some ((n,t), e)) args es
  |> Misc.map_partial id 
  |> List.split 

let cons_of_call me loc i j grd (env, st, tago) (lvo, frt, es) ns =
  let args      = frt |> Ct.args_of_refcfun |> List.map (Misc.app_fst FA.name_of_string) in
  let lsubs     = lsubs_of_annots ns in
  let args, es  = bindings_of_call loc args es in
  let subs      = List.combine (List.map fst args) es in
 
  let ist,ost   = Ct.stores_of_refcfun frt |> Misc.map_pair (rename_store lsubs subs) in
  let oast,ocst = Ct.refstore_partition Sloc.is_abstract ost in

  let tag       = CF.tag_of_instr me i j     loc in
  let tag'      = CF.tag_of_instr me i (j+1) loc in
  let ecrs      = List.map (fun e -> FI.t_exp env (CF.ctype_of_expr me e) e) es in
  let cs1,_     = FI.make_cs_tuple env grd lsubs subs ecrs (List.map snd args) None tag loc in 
  
  let cs2,_     = FI.make_cs_refstore env grd st   ist true  None tag  loc in
  let cs3,_     = FI.make_cs_refstore env grd oast st  false None tag' loc in
  let ds3       = [FI.make_dep false (Some tag') None] in 

  let st'                 = RS.upd st ocst in
  let env', cs4, ds4, wfs = env_of_retbind me loc grd tag' lsubs subs env st' lvo (Ct.ret_of_refcfun frt) in
  let wld', cs5           = instantiate_poly_clocs me env grd loc tag' (env', st', Some tag') ns in 
  wld', (cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5, ds3), wfs

let cons_of_ptrcall me loc i j grd ((env, sto, tago) as wld) (lvo, e, es) ns = match e with
  (* v := ( *f )(...), where v is local *)
  | Lval (Var v, NoOffset) when not v.Cil.vglob ->
      begin match v |> FA.name_of_varinfo |> FI.t_name env with
        | Ct.Ref (l, _) ->
          let tag           = CF.tag_of_instr me i j loc in
          let wld, cs1, wfs = cons_of_call me loc i j grd wld (lvo, RS.Function.find sto l, es) ns in
          let cs2           = cons_of_mem me loc tago tag grd env v in
            (wld, cs1 +++ cs2, wfs)
        | _ -> assert false
      end
  | _ -> assert false

(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let with_wfs (cs, ds) wfs =
  (cs, ds, wfs)

let cons_of_annotinstr me i grd (j, pre_ffm, wld) (annots, ffm, instr) =
  let env, _, _  = wld in
  let gs, is, ns = group_annots annots in
  let loc        = get_instrLoc instr in
  let tagj       = CF.tag_of_instr me i j loc in
  let wld, acds  = cons_of_annots me loc tagj grd wld pre_ffm (gs ++ is) in
  match instr with 
  | Set (lv, e, _) ->
      let _        = asserts (ns = []) "cons_of_annotinstr: new-in-set" in
      let wld, cds = cons_of_set me loc tagj grd ffm wld (lv, e) in
      (j+1, ffm, wld), with_wfs (cds +++ acds) []
  | Call (None, Lval (Var fv, NoOffset), _, _) when CilMisc.isVararg fv.Cil.vtype ->
      let _ = Cil.warnLoc loc "Ignoring vararg call" in
      (j+1, ffm, wld), with_wfs acds []
  | Call (lvo, Lval ((Var fv), NoOffset), es, _) ->
      let wld, cds, wfs = cons_of_call me loc i j grd wld (lvo, FI.ce_find_fn fv.Cil.vname env, es) ns in
      (j+2, ffm, wld), with_wfs (cds +++ acds) wfs
  | Call (lvo, Lval (Mem e, NoOffset), es, _) ->
      let wld, cds, wfs = cons_of_ptrcall me loc i j grd wld (lvo, e, es) ns in
      (j+2, ffm, wld), with_wfs (cds +++ acds) wfs
  | _ -> 
      E.s <| E.error "TBD: cons_of_instr: %a \n" d_instr instr

let scalarcons_of_binding me loc tag (j, env) grd j v cr =
  let _      = Pretty.printf "scalarcons_of_binding: [v=%s] [cr=%a] \n" v.Cil.vname Ct.d_refctype cr in
  let cr'    = FI.t_fresh Ct.scalar_ctype in
  let cs, ds = FI.make_cs env grd cr cr' None tag loc in
  (j+1, extend_env me v cr env), (cs, ds, [(v, cr')])

let scalarcons_of_instr me i grd (j, env) instr = 
  let _   = if mydebug then (ignore <| Pretty.printf "scalarcons_of_instr: %a \n" d_instr instr) in
  let loc = get_instrLoc instr in
  let tag = CF.tag_of_instr me i j loc in 
  match instr with
  | Set ((Var v, NoOffset), e, _) 
    when (not v.Cil.vglob) && CM.is_pure_expr e && CM.is_local_expr e ->
      FI.t_exp_scalar v e 
      |> scalarcons_of_binding me loc tag (j, env) grd j v 

  | Set ((Var v, NoOffset), _, _) 
    when CM.is_reference v.Cil.vtype ->
      FI.t_scalar_zero
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Call (Some (Var v, NoOffset), Lval (Mem _, NoOffset), _, _) ->
      (* Nothing we can do here; we'll have to check this call is ok after we infer
         the contents of memory. *)
      (if CM.is_reference v.Cil.vtype then FI.t_scalar_zero else FI.t_true Ct.scalar_ctype)
      |> scalarcons_of_binding me loc tag (j, env) grd j v    
     
  | Call (Some (Var v, NoOffset), Lval (Var fv, NoOffset), es, _) ->
      let farg = Ct.args_of_refcfun <+> List.map (FA.name_of_string <.> fst) <+> Misc.flip List.combine es in
      let fret = Ct.ret_of_refcfun  <+> FI.t_scalar_refctype in
      env
      |> FI.ce_find_fn fv.Cil.vname
      |> (farg <*> fret)
      |> Misc.uncurry FI.t_subs_exps 
      >> E.log "SCALAR CALLASGN: v=%s  cr=%a \n" v.Cil.vname Ct.d_refctype 
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Set ((Var v, NoOffset), _, _) 
    when (not v.Cil.vglob)  ->
      FI.t_true Ct.scalar_ctype
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Set (_,_,_) | Call (_ , _, _, _) ->
      (j+1, env), ([], [], [])

  | _ -> 
      E.s <| E.error "TBD: scalarcons_of_instr: %a \n" d_instr instr

(****************************************************************************)
(********************** Constraints for [stmt] ******************************)
(****************************************************************************)

let cons_of_ret me loc i grd (env, st, tago) e_o =
  let tag    = CF.tag_of_instr me i 1000 loc in
  let frt    = FI.ce_find_fn (CF.get_fname me) env in
  let st_cds = let _, ost = Ct.stores_of_refcfun frt in
               (FI.make_cs_refstore env grd st ost true tago tag loc) in
  let rv_cds = match e_o with None -> ([], []) 
               | Some e -> let lhs = FI.t_exp env (CF.ctype_of_expr me e) e in 
                           let rhs = Ct.ret_of_refcfun frt in
                           (FI.make_cs env grd lhs rhs tago tag loc) in
  (st_cds +++ rv_cds) 

let cons_of_annotstmt me loc i grd wld (anns, (ffm, ffms), stmt) = 
  match stmt.skind with
  | Instr is ->
      asserts (List.length anns = List.length is) "cons_of_stmt: bad annots instr";
      let (n, _, wld), cdws     =  Misc.combine3 anns ffms is 
                                |> Misc.mapfold (cons_of_annotinstr me i grd) (1, ffm, wld) in
      let cs1, ds1, ws          = Misc.splitflatten3 cdws in  
      (wld, cs1, ds1, ws)
  | Return (e_o, loc) ->
      asserts (List.length anns = 0) "cons_of_stmt: bad annots return";
      let cs, ds        = cons_of_ret me loc i grd wld e_o in
      (wld, cs, ds, [])
  | _ ->
      let _ = if !Cs.safe then E.error "unknown annotstmt: %a" d_stmt stmt in
      (wld, [], [], [])

let scalarcons_of_stmt me i grd env stmt = 
  match stmt.skind with
  | Instr is -> 
      let (_, env), cds = Misc.mapfold (scalarcons_of_instr me i grd) (1, env) is in
      let cs, ds, des   = Misc.splitflatten3 cds in
      env, cs, des, ds
  | _ -> env, [], [], [] 

(****************************************************************************)
(********************** Constraints for (cfg)block **************************)
(****************************************************************************)

let wcons_of_block me loc (_, sto, _) i des =
  let _    = if mydebug then Printf.printf "wcons_of_block: %d \n" i in 
  let csto = if CF.has_shape me then CF.csto_of_block me i else RS.empty in
  let tag  = CF.tag_of_instr me i 0 loc in
  let phis = CF.phis_of_block me i in
  let env  = CF.inenv_of_block me i in
  let wenv = phis |> List.fold_left (weaken_undefined me true) env in
  let ws   = phis |> List.map  (fun v -> FI.ce_find  (FA.name_of_varinfo v) env) 
                  |> Misc.flap (fun cr -> FI.make_wfs wenv sto cr tag) in
  let ws'  = FI.make_wfs_refstore wenv (RS.upd sto csto) csto tag in
  let ws'' = des |> Misc.flap (fun (v, cr) -> FI.make_wfs wenv sto cr tag) in
  ws ++ ws' ++ ws''

let cons_of_block me i =
  let _                = if mydebug then Printf.printf "cons_of_block: %d \n" i in 
  let loc              = CF.location_of_block me i in
  let grd              = CF.guard_of_block me i None in
  let astmt            = CF.annotstmt_of_block me i in
  let wld              = CF.inwld_of_block me i in
  let ws1              = wcons_of_block me loc wld i [] in
  let wld, cs, ds, ws2 = cons_of_annotstmt me loc i grd wld astmt in
  (wld, (ws1 ++ ws2, cs, [], ds))

let scalarcons_of_block me i = 
  let _             = if mydebug then Printf.printf "scalarcons_of_block: %d \n" i in 
  let loc           = CF.location_of_block me i in
  let grd           = CF.guard_of_block me i None in
  let stmt          = CF.stmt_of_block me i in
  let wld           = CF.inwld_of_block me i in
  let env,cs,des,ds = scalarcons_of_stmt me i grd (fst3 wld) stmt in
  let ws            = wcons_of_block me loc wld i des in
  (withfst3 wld env, (ws, cs, des, ds))

let cons_of_block me = if CF.has_shape me then cons_of_block me else scalarcons_of_block me

(****************************************************************************)
(********************** Constraints for (cfg)edge  **************************)
(****************************************************************************)

(* {{{
let tcons_of_phis me phia =
  let iasgns = Misc.array_to_index_list phia in 
  Misc.flap_pair begin fun (i, asgns) ->
    let envi,_,_   = CF.outwld_of_block me i in
    let asgns'     = Misc.transpose asgns in
    Misc.flap_pair begin fun (j, vvjs) ->
      let pj       = CF.guard_of_block me j (Some i) in
      let locj     = CF.location_of_block me j in
      let tagj     = CF.tag_of_instr me j 0 locj in
      let envj,_,_ = CF.outwld_of_block me j in
      let nnjs     = Misc.map (Misc.map_pair FI.name_of_varinfo) vvjs in
      Misc.flap_pair begin fun (v, vj) ->
        let envj   = weaken_undefined me false envj v in
        let n, nj  = Misc.map_pair FI.name_of_varinfo (v, vj) in
        let lhs    = if not (CF.is_undefined me vj) then FI.t_name envj nj else  
                       FI.ce_find nj envj |> FI.ctype_of_refctype |> FI.t_true in
        let rhs    = FI.ce_find n envi |> FI.t_subs_names nnjs in
        FI.make_cs envj pj lhs rhs None tagj locj 
      end vvjs 
    end asgns' 
  end iasgns 
}}} *)

let var_cons_of_edge me envi loci tagi grdij envj subs vjvis =
  Misc.flap_pair begin fun (vj, vi) ->
    let envi = weaken_undefined me false envi vj in
    let lhs  = let ni = FA.name_of_varinfo vi in
               if not (CF.is_undefined me vi) then FI.t_name envi ni else  
                  FI.ce_find ni envi |> Ct.ctype_of_refctype |> FI.t_true in
    let rhs  = let nj = FA.name_of_varinfo vj in
               FI.ce_find nj envj |> FI.t_subs_names subs in
    FI.make_cs envi grdij lhs rhs None tagi loci
  end vjvis

let gen_cons_of_edge me iwld' loci tagi grdij i j =
  CF.annots_of_edge me i j 
  |> cons_of_annots me loci tagi grdij iwld' Sloc.SlocMap.empty
  |> snd

let join_cons_of_edge me (envi, isto', _) loci tagi grdij subs i j =
  let rsto   = CF.csto_of_block me j |> FI.refstore_subs FI.t_subs_names subs  in 
  let lsto,_ = Ct.refstore_partition (fun cl -> RS.mem rsto cl) isto' in
  FI.make_cs_refstore envi grdij lsto rsto true None tagi loci

let cons_of_edge me i j =
  let _     = if mydebug then Printf.printf "cons_of_edge: %d --> %d \n" i j in 
  let iwld' = CF.outwld_of_block me i in
  let loci  = CF.location_of_block me i in
  let tagi  = CF.tag_of_instr me i 0 loci in
  let grdij = CF.guard_of_block me i (Some j) in
  let envj  = CF.outwld_of_block me j |> fst3 in
  let vjvis = CF.asgns_of_edge me i j in
  let subs  = List.map (Misc.map_pair FA.name_of_varinfo) vjvis in
  (var_cons_of_edge me (fst3 iwld') loci tagi grdij envj subs vjvis) +++
  (gen_cons_of_edge me iwld' loci tagi grdij i j) +++
  (join_cons_of_edge me iwld' loci tagi grdij subs i j)

let scalarcons_of_edge me i j = 
  let _     = if mydebug then Printf.printf "scalarcons_of_edge: %d --> %d \n" i j in 
  let iwld' = CF.outwld_of_block me i in
  let loci  = CF.location_of_block me i in
  let tagi  = CF.tag_of_instr me i 0 loci in
  let grdij = CF.guard_of_block me i (Some j) in
  let envj  = CF.outwld_of_block me j |> fst3 in
  let vjvis = CF.asgns_of_edge me i j in
  let subs  = List.map (Misc.map_pair FA.name_of_varinfo) vjvis in
  (var_cons_of_edge me (fst3 iwld') loci tagi grdij envj subs vjvis)

let cons_of_edge me = if CF.has_shape me then cons_of_edge me else scalarcons_of_edge me

(****************************************************************************)
(********************** Constraints for ST.ssaCfgInfo ***********************)
(****************************************************************************)

let process_block me i =
  let wld, x = cons_of_block me i in
  me |> CF.add_wld i wld |> CF.add_cons x

let process_block_succs me i =
  List.fold_left begin fun me j -> 
    let cs, ds = cons_of_edge me i j in
    CF.add_cons ([], cs, [], ds) me
  end me (CF.succs_of_block me i) 

let log_of_sci sci sho = 
  if Cs.ck_olev Cs.ol_solve then
    let _ = Pretty.printf "cons_of_sci: %s \n" sci.ST.fdec.Cil.svar.Cil.vname in
    match sho with None -> () | Some sh ->
      let _ = Pretty.printf "%a\n" Refanno.d_block_annotation_array sh.Sh.anna in
      let _ = Pretty.printf "%a\n" Refanno.d_conca sh.Sh.conca in
      let _ = Pretty.printf "%a" Refanno.d_ctab sh.Sh.theta in 
      let _ = Pretty.printf "ICstore = %a\n" Ct.I.Store.d_store_addrs sh.Sh.store in
      ()

let cons_of_sci tgr gnv gst sci sho =
  let _  = log_of_sci sci sho in
  let is = ST.reachable_blocks_of_sci sci in 
  CF.create tgr gnv gst sci sho
  |> Misc.flip (List.fold_left process_block) is
  |> Misc.flip (List.fold_left process_block_succs) is 
  |> CF.get_cons

(******************************************************************************)
(************** Generate Constraints for Each Function and Global *************)
(******************************************************************************)

let cons_of_global_store tgr gst =
  let tag   = CilTag.make_global_t tgr Cil.locUnknown in
  let ws    = FI.make_wfs_refstore FI.ce_empty gst gst tag in
  let zst   = RS.Data.map FI.t_zero_refctype gst in
  let cs, _ = FI.make_cs_refstore FI.ce_empty Ast.pTrue zst gst false None tag Cil.locUnknown in
  (ws, cs)

let type_of_init v vtyp = function
  | Some (SingleInit e)        -> FI.t_exp FI.ce_empty (Ct.ctype_of_refctype vtyp) e
  | Some (CompoundInit (t, _)) -> t |> CilMisc.bytesSizeOf |> FI.t_size_ptr (Ct.ctype_of_refctype vtyp)
  | None                       ->
      let ct = Ct.ctype_of_refctype vtyp in
      match Cil.unrollType v.vtype with
      | TArray (t, (Some len as leno), _) ->
          Cil.lenOfArray leno * CilMisc.bytesSizeOf t |> FI.t_size_ptr ct
      | TPtr (t, _) ->
          t |> CilMisc.bytesSizeOf |> FI.t_size_ptr ct
      | _ ->
          FI.t_true ct

let add_offset loc t ctptr off =
  match ctptr with
  | Ct.Ref (s, (i, r)) ->
      Ct.Ref (s, (off |> CilMisc.bytesOffset t |> Ct.Index.of_int |> Ct.Index.plus i, r))
  | _ -> halt <| errorLoc loc "Adding offset to bogus type: %a\n\n" Ct.d_refctype ctptr

let rec cons_of_init (sto, cs) tag loc env cloc t ctptr = function
  | SingleInit e ->
      let cr  = Ct.refstore_read loc sto ctptr in
      let ct  = Ct.ctype_of_refctype cr in
      let cr' = FI.t_exp env ct e in
        if Ct.is_soft_ptr loc sto ctptr then
          (sto, cs ++ (FI.make_cs env Ast.pTrue cr' cr None tag loc |> fst))
        else
          (Ct.refstore_write loc sto ctptr cr', cs)
  | CompoundInit (_, inits) ->
      foldLeftCompound
        ~implicit:true
        ~doinit:(fun off init t' acc -> cons_of_init acc tag loc env cloc t' (add_offset loc t ctptr off) init)
        ~ct:t
        ~initl:inits
        ~acc:(sto, cs)

let cons_of_var_init tag loc sto v vtyp inito =
  let cs1, _ = FI.make_cs FI.ce_empty Ast.pTrue (type_of_init v vtyp inito) vtyp None tag loc in
  match inito with
  | Some (CompoundInit _ as init) ->
      let aloc, r     = match vtyp with Ct.Ref (al, r) -> (al, r) | _ -> assert false in
      let cloc        = Sloc.fresh_concrete aloc in
      let ctptr       = Ct.Ref (cloc, r) in
      let env, sto, _ = fst <| FI.extend_world sto aloc cloc false id loc tag (FI.ce_empty, sto, None) in 
      let sto, cs2    = cons_of_init (sto, []) tag loc env cloc v.vtype ctptr init in
      let ld1         = (cloc, Ct.refstore_get sto cloc) in
      let ld2         = (aloc, Ct.refstore_get sto aloc) in
      let cs3, _      = FI.make_cs_refldesc env Ast.pTrue ld1 ld2 None tag loc in
      cs1 ++ cs2 ++ cs3
  | _ -> cs1

(************************************************************************************)
(******************************** API ***********************************************)
(************************************************************************************)

(* API *)
let cons_of_decs tgr spec gnv gst decs =
  let ws, cs = cons_of_global_store tgr gst in
  List.fold_left begin fun (ws, cs, _, _) -> function
    | CM.FunDec (fn, loc) ->
        let tag     = CilTag.make_t tgr loc fn 0 0 in
        let irf     = FI.ce_find_fn fn gnv in
        let ws'     = FI.make_wfs_fn gnv irf tag in
        let srf, b  = CS.get_fun fn spec in
        let cs',ds' = if b then FI.make_cs_refcfun gnv Ast.pTrue irf srf tag loc else ([],[]) in
        (ws' ++ ws, cs' ++ cs, [], [])
    | CM.VarDec (v, loc, init) ->
        let tag     = CilTag.make_global_t tgr loc in
        let vtyp    = FI.ce_find (FA.name_of_string v.vname) gnv in
        let vspctyp = let vsp, chk = CS.get_var v.vname spec in 
                      if chk then vsp else FI.t_true_refctype vtyp in
        let cs'     = cons_of_var_init tag loc gst v vtyp init in
        let cs'', _ = FI.make_cs FI.ce_empty Ast.pTrue vtyp vspctyp None tag loc in
        let ws'     = FI.make_wfs FI.ce_empty gst vtyp tag in
          (ws' ++ ws, cs'' ++ cs' ++ cs, [], [])
  end (ws, cs, [], []) decs

(* API *)
let cons_of_scis tgr gnv gst scim shmo ci =
  SM.fold begin fun fn sci ci ->
    let _ = if mydebug then ignore(Pretty.printf "Generating Constraints for %s \n" fn) in
    shmo
    |> Misc.maybe_map (SM.find sci.ST.fdec.svar.vname)
    |> cons_of_sci tgr gnv gst sci 
    |> Consindex.add ci fn sci
  end scim ci 
