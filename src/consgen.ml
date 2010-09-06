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
module CF = Consinfra
module IM = Misc.IntMap
module SM = Misc.StringMap
module SS = Misc.StringSet
module M  = Misc
module P  = Pretty
module CM = CilMisc

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

let rename_refctype lsubs subs cr =
  cr |> FI.t_subs_locs lsubs
     |> FI.t_subs_exps subs

let rename_store lsubs subs sto =
  sto |> FI.refstore_subs_locs lsubs 
      |> FI.refstore_subs FI.t_subs_exps subs 
      |> FI.RefCTypes.Store.subs lsubs

(*
let rename_store lsubs subs st = 
  st |> Ctypes.prestore_subs lsubs
     |> Ctypes.prestore_map_ct (rename_refctype lsubs subs)
*)

let weaken_undefined me rm env v = 
  let n = FI.name_of_varinfo v in
  let b = FI.ce_mem n env && CF.is_undefined me v in
  if not b then env else
    if rm then FI.ce_rem n env else
      let r    = FI.ce_find n env |> FI.t_true_refctype in
      let env' = FI.ce_rem n env in
      FI.ce_adds env' [(n,r)]

(****************************************************************************)
(********************** Constraints for Annots ******************************)
(****************************************************************************)

let cons_of_annot me loc tag grd (env, sto, tago) = function 
  | Refanno.Gen  (cloc, aloc) ->
      let _      = CM.assertLoc loc (FI.refstore_mem cloc sto) "cons_of_annot: (Gen)!" in
      let sto'   = FI.refstore_remove cloc sto in
      let ld1    = (cloc, FI.refstore_get sto cloc) in
      let ld2    = (aloc, FI.refstore_get sto aloc) in
      let cf     = CF.get_alocmap me in
      let cds    = FI.make_cs_refldesc cf env grd ld1 ld2 tago tag loc in
      ((env, sto', tago), cds)

  | Refanno.WGen  (cloc, aloc) ->
      let _      = CM.assertLoc loc (FI.refstore_mem cloc sto) "cons_of_annot: (WGen)!" in
      let sto'   = FI.refstore_remove cloc sto in
      ((env, sto', tago), ([],[]))

  | Refanno.Ins (aloc, cloc) ->
      let _      = CM.assertLoc loc (not (FI.refstore_mem cloc sto)) "cons_of_annot: (Ins)!" in
      let cf     = CF.get_alocmap me in
      let wld',_ = FI.extend_world cf sto aloc cloc false loc tag (env, sto, tago) in 
      (wld', ([], []))

  | _ -> assertf "cons_of_annot: New/NewC" 
  
let cons_of_annots me loc tag grd wld annots =
  Misc.mapfold (cons_of_annot me loc tag grd) wld annots
  |> Misc.app_snd Misc.splitflatten 

(******************************************************************************)
(*********************** Constraints for Deferred Checks **********************)
(******************************************************************************)

let cons_of_dcheck me loc grd tag (env, _, tago) (v, rct) =
  let cf  = CF.get_alocmap me in
  let vct = v |> FI.name_of_varinfo |> FI.t_name env in
    FI.make_cs cf env grd vct rct tago tag loc

(****************************************************************************)
(********************** Constraints for Assignments *************************)
(****************************************************************************)

let extend_env me v cr env =
  let ct = CF.ctype_of_varinfo me v in
  let cr = FI.t_ctype_refctype ct cr in
(*let _  = Pretty.printf "extend_env: v = %s, ct = %a, cr = %a \n" 
           v.Cil.vname Ctypes.d_ctype ct FI.d_refctype cr in
*)FI.ce_adds env [(FI.name_of_varinfo v), cr]

let cons_of_mem me loc tago tag grd env v =
  if !Constants.manual then
    ([], [])
  else
    let rct = v |> FI.name_of_varinfo |> FI.t_name env in
    let cf  = CF.get_alocmap me in  
    FI.make_cs_validptr cf env grd rct tago tag loc

let cons_of_rval me loc tag grd (env, sto, tago) = function
  (* *v *)
  | Lval (Mem (Lval (Var v', NoOffset)), _)
  | Lval (Mem (CastE (_, Lval (Var v', NoOffset))), _) ->
      (FI.ce_find (FI.name_of_varinfo v') env |> FI.refstore_read loc sto,
      cons_of_mem me loc tago tag grd env v')
  | Lval (Var v, NoOffset) when v.vglob ->
      (FI.ce_find (FI.name_of_varinfo v) env, ([], []))
  (* e, where e is pure *)
  | e ->
      let _  = CilMisc.check_pure_expr e in
      (FI.t_exp env (CF.ctype_of_expr me e) e, ([], []))

let cons_of_set me loc tag grd (env, sto, tago) = function
  (* v := e, where v is local *)
  | (Var v, NoOffset), rv when not v.Cil.vglob ->
      let cr, cds = cons_of_rval me loc tag grd (env, sto, tago) rv in
        (extend_env me v cr env, sto, Some tag), cds

  (* v := e, where v is global *)
  | (Var v, NoOffset), rv when v.Cil.vglob ->
      let cr, (cs1, _) = cons_of_rval me loc tag grd (env, sto, tago) rv in
      let cf           = CF.get_alocmap me in
      let cs2, _       = FI.make_cs cf env grd cr (CF.refctype_of_global me v) tago tag loc in
        (env, sto, Some tag), (cs1 ++ cs2, [])

  (* *v := e, where e is pure *)
  | (Mem (Lval(Var v, NoOffset)), _), e 
  | (Mem (CastE (_, Lval (Var v, NoOffset))), _), e ->
      let addr = if v.vglob then CF.refctype_of_global me v else FI.ce_find (FI.name_of_varinfo v) env in
      let cr'  = FI.t_exp env (CF.ctype_of_expr me e) e in
      let cs1, ds1 = cons_of_mem me loc tago tag grd env v in
      let isp  = try FI.is_soft_ptr loc sto addr with ex ->
                   Errormsg.s <| Cil.errorLoc loc "is_soft_ptr crashes on %s" v.vname in
      if isp (* FI.is_soft_ptr loc sto addr *) then
        let cr       = FI.refstore_read loc sto addr in
        let cf       = CF.get_alocmap me in
        let cs2, ds2 = FI.make_cs cf env grd cr' cr tago tag loc in
        (env, sto, Some tag), (cs1 ++ cs2, ds1 ++ ds2)
      else
        let sto' = FI.refstore_write loc sto addr cr' in
        (env, sto', Some tag), (cs1, ds1)

  | _ -> assertf "TBD: cons_of_set"

let cons_of_set me loc tag grd (env, sto, tago) ((lv, e) as x) = 
  Misc.do_catchu (cons_of_set me loc tag grd (env, sto, tago)) x 
    (fun ex -> E.error "(%s) cons_of_set [%a] : %a := %a \n" 
              (Printexc.to_string ex) d_loc loc d_lval lv d_exp e)

(****************************************************************************)
(********************** Constraints for Calls *******************************)
(****************************************************************************)

let cons_of_tuple cf env grd lsubs subs cr1s cr2s tago tag loc =
  Misc.map2 begin fun cr1 cr2 ->
    FI.make_cs cf env grd cr1 (rename_refctype lsubs subs cr2) tago tag loc
  end cr1s cr2s 
  |> Misc.splitflatten

let env_of_retbind me lsubs subs env lvo cr = 
  match lvo with 
  | Some ((Var v), NoOffset) -> extend_env me v (rename_refctype lsubs subs cr) env
  | None                     -> env
  | _  when !Constants.safe  -> assertf "env_of_retbind"
  | _                        -> env

let instantiate_poly_clocs me env grd loc tag' ((_, st',_) as wld) ns =
  let asto = CF.get_astore me in
  let cf   = CF.get_alocmap me in
  ns |> Misc.map_partial (function Refanno.NewC (_,al,cl) -> Some (al,cl) | _ -> None)
     |> List.filter (snd <+> FI.is_poly_cloc st')
     |> Misc.mapfold (fun wld (al, cl) -> FI.extend_world cf asto al cl true loc tag' wld) wld
     |> Misc.app_snd List.flatten

let cons_of_call me loc i j grd (env, st, tago) (lvo, fn, es) ns =
  let frt       = FI.ce_find_fn fn env in
  let args      = FI.args_of_refcfun frt |> List.map (Misc.app_fst FI.name_of_string) in
  let lsubs     = lsubs_of_annots ns in
  let subs      = try List.combine (List.map fst args) es 
                  with _ -> (Errormsg.s <| Cil.errorLoc loc "cons_of_call: bad params") in

  let ist,ost   = FI.stores_of_refcfun frt |> Misc.map_pair (rename_store lsubs subs) in
  let oast,ocst = FI.refstore_partition Sloc.is_abstract ost in

  let tag       = CF.tag_of_instr me i j     loc in
  let tag'      = CF.tag_of_instr me i (j+1) loc in
  let ecrs      = List.map (fun e -> FI.t_exp env (CF.ctype_of_expr me e) e) es in
  let cf        = CF.get_alocmap me in
  let cs1,_     = cons_of_tuple cf env grd lsubs subs ecrs (List.map snd args) None tag loc in 
  
  let cs2,_     = FI.make_cs_refstore cf env grd st   ist true  None tag  loc in
  let cs3,_     = FI.make_cs_refstore cf env grd oast st  false None tag' loc in
  let ds3       = [FI.make_dep false (Some tag') None] in 

  let env'      = env_of_retbind me lsubs subs env lvo (FI.ret_of_refcfun frt) in
  let st'       = FI.RefCTypes.Store.upd st ocst in
  let wld', cs4 = instantiate_poly_clocs me env grd loc tag' (env', st', Some tag') ns in 
  wld', (cs1 ++ cs2 ++ cs3 ++ cs4, ds3)

(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let cons_of_annotinstr me i grd (j, wld) (annots, dcks, instr) =
  let gs, is, ns = group_annots annots in
  let loc        = get_instrLoc instr in
  let tagj       = CF.tag_of_instr me i j loc in
  let wld, acds  = cons_of_annots me loc tagj grd wld (gs ++ is) in
  let cks        = dcks |> List.map (cons_of_dcheck me loc grd tagj wld) |> Misc.splitflatten in
  let cds'       = acds +++ cks in
  match instr with 
  | Set (lv, e, _) ->
      let _        = asserts (ns = []) "cons_of_annotinstr: new-in-set" in
      let wld, cds = cons_of_set me loc tagj grd wld (lv, e) in
      (j+1, wld), cds +++ cds'
  | Call (None, Lval (Var fv, NoOffset), _, _) when CilMisc.isVararg fv.Cil.vtype ->
      let _ = Cil.warnLoc loc "Ignoring vararg call" in
        (j+1, wld), cds'
  | Call (lvo, Lval ((Var fv), NoOffset), es, _) ->
      let wld, cds = cons_of_call me loc i j grd wld (lvo, fv.Cil.vname, es) ns in
      (j+2, wld), cds +++ cds'
  | _ -> 
      E.s <| E.error "TBD: cons_of_instr: %a \n" d_instr instr

(****************************************************************************)
(********************** Constraints for [stmt] ******************************)
(****************************************************************************)

let cons_of_ret me loc i grd (env, st, tago) e_o =
  let tag    = CF.tag_of_instr me i 1000 loc in
  let frt    = FI.ce_find_fn (CF.get_fname me) env in
  let cf     = CF.get_alocmap me in
  let st_cds = let _, ost = FI.stores_of_refcfun frt in
               (FI.make_cs_refstore cf env grd st ost true tago tag loc) in
  let rv_cds = match e_o with None -> ([], []) 
               | Some e -> let lhs = FI.t_exp env (CF.ctype_of_expr me e) e in 
                           let rhs = FI.ret_of_refcfun frt in
                           (FI.make_cs cf env grd lhs rhs tago tag loc) in
  (st_cds +++ rv_cds) 

let cons_of_annotstmt me loc i grd wld (anns, dckss, stmt) = 
  match stmt.skind with
  | Instr is ->
      (* INTRA-FOLD: let ann, anns = Misc.list_snoc anns in *)
      asserts (List.length anns = List.length is) "cons_of_stmt: bad annots instr";
      let (n, wld), cds   =  Misc.combine3 anns dckss is 
                          |> Misc.mapfold (cons_of_annotinstr me i grd) (1, wld) in
      let cs1, ds1        = Misc.splitflatten cds in  
      (* INTRA-FOLD: let wld, (cs2, ds2) = cons_of_annots me loc (CF.tag_of_instr me i n loc) grd wld ann in *)
      (wld, (* INTRA-FOLD: cs2 ++ *) cs1, (* INTRA-FOLD: ds2 ++ *) ds1)
  | Return (e_o, loc) ->
      asserts (List.length anns = 0) "cons_of_stmt: bad annots return";
      let cs, ds        = cons_of_ret me loc i grd wld e_o in
      (wld, cs, ds)
  | _ ->
      let _ = if !Constants.safe then E.error "unknown annotstmt: %a" d_stmt stmt in
      (wld, [], [])

(****************************************************************************)
(********************** Constraints for (cfg)block **************************)
(****************************************************************************)

let wcons_of_block me loc i =
  let _    = if mydebug then Printf.printf "wcons_of_block: %d \n" i in 
  let cf   = CF.get_alocmap me in
  let tag  = CF.tag_of_instr me i 0 loc in
  let phis = CF.phis_of_block me i in
  let env  = CF.inenv_of_block me i in
  let wenv = phis |> List.fold_left (weaken_undefined me true) env in
  let ws   = phis |> List.map  (fun v -> FI.ce_find  (FI.name_of_varinfo v) env) 
                  |> Misc.flap (fun cr -> FI.make_wfs cf wenv cr tag) in
  let ws'  = FI.make_wfs_refstore cf wenv (CF.csto_of_block me i) tag in
  ws ++ ws'

let cons_of_block me i =
  let _           = if mydebug then Printf.printf "cons_of_block: %d \n" i in 
  let loc         = CF.location_of_block me i in
  let grd         = CF.guard_of_block me i None in
  let astmt       = CF.annotstmt_of_block me i in
  let wld         = CF.inwld_of_block me i in
  let ws          = wcons_of_block me loc i in
  let wld, cs, ds = cons_of_annotstmt me loc i grd wld astmt in
  (wld, (ws, cs, ds))

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
    let cf   = CF.get_alocmap me in
    let envi = weaken_undefined me false envi vj in
    let lhs  = let ni = FI.name_of_varinfo vi in
               if not (CF.is_undefined me vi) then FI.t_name envi ni else  
                  FI.ce_find ni envi |> FI.ctype_of_refctype |> FI.t_true in
    let rhs  = let nj = FI.name_of_varinfo vj in
               FI.ce_find nj envj |> FI.t_subs_names subs in
    FI.make_cs cf envi grdij lhs rhs None tagi loci
  end vjvis

let gen_cons_of_edge me iwld' loci tagi grdij i j =
  CF.annots_of_edge me i j 
  |> cons_of_annots me loci tagi grdij iwld'
  |> snd

let join_cons_of_edge me (envi, isto', _) loci tagi grdij subs i j = 
  let rsto   = CF.csto_of_block me j |> FI.refstore_subs FI.t_subs_names subs  in 
  let lsto,_ = FI.refstore_partition (fun cl -> FI.refstore_mem cl rsto) isto' in
  let cf     = CF.get_alocmap me in
  FI.make_cs_refstore cf envi grdij lsto rsto true None tagi loci

let cons_of_edge me i j =
  let _     = if mydebug then Printf.printf "cons_of_edge: %d --> %d \n" i j in 
  let iwld' = CF.outwld_of_block me i in
  let loci  = CF.location_of_block me i in
  let tagi  = CF.tag_of_instr me i 0 loci in
  let grdij = CF.guard_of_block me i (Some j) in
  let envj  = CF.outwld_of_block me j |> fst3 in
  let vjvis = CF.asgns_of_edge me i j in
  let subs  = List.map (Misc.map_pair FI.name_of_varinfo) vjvis in
  (var_cons_of_edge me (fst3 iwld') loci tagi grdij envj subs vjvis) +++
  (gen_cons_of_edge me iwld' loci tagi grdij i j) +++
  (join_cons_of_edge me iwld' loci tagi grdij subs i j)

(****************************************************************************)
(********************** Constraints for ST.ssaCfgInfo ***********************)
(****************************************************************************)

let process_block me i =
  let wld, x = cons_of_block me i in
  me |> CF.add_wld i wld |> CF.add_cons x

let process_block_succs me i =
  List.fold_left begin fun me j -> 
    let cs, ds = cons_of_edge me i j in
    CF.add_cons ([], cs, ds) me
  end me (CF.succs_of_block me i) 

let log_of_sci sci shp = 
  if Constants.ck_olev Constants.ol_solve then
    let _ = Pretty.printf "cons_of_sci: %s \n" sci.ST.fdec.Cil.svar.Cil.vname in
    let _ = Pretty.printf "%a\n" Refanno.d_block_annotation_array shp.Inferctypes.anna in
    let _ = Pretty.printf "%a\n" Refanno.d_conca shp.Inferctypes.conca in
    let _ = Pretty.printf "%a" Refanno.d_ctab shp.Inferctypes.theta in 
    let _ = Pretty.printf "ICstore = %a\n" Ctypes.I.Store.d_store_addrs shp.Inferctypes.store in
    ()

let cons_of_sci tgr gnv gst sci shp =
  let _  = log_of_sci sci shp in
  let is = ST.reachable_blocks_of_sci sci in 
  CF.create tgr gnv gst sci shp
  |> Misc.flip (List.fold_left process_block) is
  |> Misc.flip (List.fold_left process_block_succs) is 
  |> CF.get_cons

(****************************************************************************)
(********************** Constraints for Public Functions ********************)
(****************************************************************************)

(* Generate constraint: 0 |- rf <: rf' 
 * where rf  = forall [...]. it/hi   -> ot/ho
 *       rf' = forall [...]. it'/hi' -> ot'/ho'
 *         0 |- it'/hi' <: it/hi
 *   it',hi' |- ot/ho <: ot'/ho' *)

let cons_of_refcfun cf loc gnv fn rf rf' tag = 
  let it, it'     = Misc.map_pair FI.args_of_refcfun (rf, rf') in
  let ocr, ocr'   = Misc.map_pair FI.ret_of_refcfun (rf, rf') in
  let hi, ho      = FI.stores_of_refcfun rf in
  let hi',ho'     = FI.stores_of_refcfun rf' in
  let env         = it' |> List.map (Misc.app_fst FI.name_of_string)
                        |> FI.ce_adds gnv in
  let ircs, ircs' = Misc.map_pair (List.map snd) (it, it') in
  (* contravariant inputs *)
      (cons_of_tuple cf env Ast.pTrue [] [] ircs' ircs None tag loc)  
  +++ (FI.make_cs_refstore cf env Ast.pTrue hi' hi true None tag loc) 
  (* covariant outputs *)
  +++ (FI.make_cs cf env Ast.pTrue ocr ocr' None tag loc)
  +++ (FI.make_cs_refstore cf env Ast.pTrue ho ho' true None tag loc)


(***************************************************************************)
(*************** Processing SCIs and Globals *******************************)
(***************************************************************************)

type dec =
  | FunDec of string * location
  | VarDec of Cil.varinfo * location * init option

let infer_shapes cil spec scis =
  let spec = FI.cspec_of_refspec spec in
    (Inferctypes.infer_shapes cil spec scis, spec |> fst3 |> SM.map fst)

let shapem_of_scim cil spec scim =
  (SM.empty, SM.empty)
  |> SM.fold begin fun fn (rf, _) (bm, fm) ->
       let cf = FI.cfun_of_refcfun rf in
       if SM.mem fn scim
       then (bm, (SM.add fn (cf, SM.find fn scim) fm))
       else (SM.add fn cf bm, fm)
     end (fst3 spec)
  >> (fst <+> Misc.sm_print_keys "builtins")
  >> (snd <+> Misc.sm_print_keys "non-builtins")
  >> (fun _ -> ignore <| E.log "\nSTART: SHAPE infer \n") 
  |> (fun (_, fm) -> infer_shapes cil spec fm)
  >> (fun _ -> ignore <| E.log "\nDONE: SHAPE infer \n") 


(* TBD: UGLY *)
let mk_gnv (funspec, varspec, storespec) cenv decs =
  let decs = decs |> Misc.map_partial (function FunDec (fn,_) -> Some fn | _ -> None)
                  |> List.fold_left (Misc.flip SS.add) SS.empty in
  let gnv0 = varspec
             |> M.sm_to_list
             |> List.map begin fun (vn, (vty, _)) -> 
                 (FI.name_of_string vn, vty |> FI.ctype_of_refctype |> FI.t_fresh) 
                end
             |> FI.ce_adds FI.ce_empty in
  M.sm_to_list cenv
  |> List.map begin fun (fn, ft) ->
       (fn, if SS.mem fn decs
            then FI.t_fresh_fn ft
            else fst (Misc.do_catch ("missing spec: "^fn) (SM.find fn) funspec))
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

let rename_funspec scim funspec = 
  SM.mapi begin fun fn (rf,b) -> 
    if SM.mem fn scim
    then (rename_args rf (SM.find fn scim), b)
    else (rf, b)
  end funspec

(******************************************************************************)
(************** Generate Constraints for Each Function and Global *************)
(******************************************************************************)


let cf0 = fun _ -> None

let cons_of_global_store tgr gst =
  let tag   = CilTag.make_global_t tgr Cil.locUnknown in
  let ws    = FI.make_wfs_refstore cf0 FI.ce_empty gst tag in
  let zst   = FI.RefCTypes.Store.map_ct FI.t_zero_refctype gst in
  let cs, _ = FI.make_cs_refstore cf0 FI.ce_empty Ast.pTrue zst gst false None tag Cil.locUnknown in
  (ws, cs)

let type_of_init v vtyp = function
  | Some (SingleInit e)        -> FI.t_exp FI.ce_empty (FI.ctype_of_refctype vtyp) e
  | Some (CompoundInit (t, _)) -> t |> CilMisc.bytesSizeOf |> FI.t_size_ptr (FI.ctype_of_refctype vtyp)
  | None                       ->
      let ct = FI.ctype_of_refctype vtyp in
        match Cil.unrollType v.vtype with
          | TArray (t, (Some len as leno), _) ->
              Cil.lenOfArray leno * CilMisc.bytesSizeOf t |> FI.t_size_ptr ct
          | TPtr (t, _) ->
              t |> CilMisc.bytesSizeOf |> FI.t_size_ptr ct
          | _ ->
              FI.t_true ct

let add_offset loc t ctptr off =
  match ctptr with
    | Ctypes.Ref (s, (i, r)) ->
        Ctypes.Ref (s, (off |> CilMisc.bytesOffset t |> Ctypes.Index.of_int |> Ctypes.Index.plus i, r))
    | _ -> halt <| errorLoc loc "Adding offset to bogus type: %a\n\n" FI.d_refctype ctptr

let rec cons_of_init (sto, cs) tag loc env cloc t ctptr = function
  | SingleInit e ->
      let cr  = FI.refstore_read loc sto ctptr in
      let ct  = FI.ctype_of_refctype cr in
      let cr' = FI.t_exp env ct e in
        if FI.is_soft_ptr loc sto ctptr then
          (sto, cs ++ (FI.make_cs cf0 env Ast.pTrue cr' cr None tag loc |> fst))
        else
          (FI.refstore_write loc sto ctptr cr', cs)
  | CompoundInit (_, inits) ->
      foldLeftCompound
        ~implicit:true
        ~doinit:(fun off init t' acc -> cons_of_init acc tag loc env cloc t' (add_offset loc t ctptr off) init)
        ~ct:t
        ~initl:inits
        ~acc:(sto, cs)

let cons_of_var_init tag loc sto v vtyp inito =
  let cs1, _ = FI.make_cs cf0 FI.ce_empty Ast.pTrue (type_of_init v vtyp inito) vtyp None tag loc in
    match inito with
      | Some (CompoundInit _ as init) ->
          let cloc        = Sloc.fresh Sloc.Concrete in
          let aloc, ctptr = match vtyp with Ctypes.Ref (al, r) -> (al, Ctypes.Ref (cloc, r)) 
                                          | _ -> assert false in
          let env, sto, _ = fst <| FI.extend_world cf0 sto aloc cloc false loc tag (FI.ce_empty, sto, None) in 
          let sto, cs2    = cons_of_init (sto, []) tag loc env cloc v.vtype ctptr init in
          let ld1         = (cloc, FI.refstore_get sto cloc) in
          let ld2         = (aloc, FI.refstore_get sto aloc) in
          let cs3, _      = FI.make_cs_refldesc cf0 env Ast.pTrue ld1 ld2 None tag loc in
          cs1 ++ cs2 ++ cs3
      | _ -> cs1

let cons_of_decs tgr (funspec, varspec, _) gnv gst decs =
  let ws, cs = cons_of_global_store tgr gst in
  List.fold_left begin fun (ws, cs, _) -> function
    | FunDec (fn, loc) ->
        let tag    = CilTag.make_t tgr loc fn 0 0 in
        let irf    = FI.ce_find_fn fn gnv in
        let ws'    = FI.make_wfs_fn cf0 gnv irf tag in
        let srf, b = SM.find fn funspec in
        let cs',ds'= if b then cons_of_refcfun cf0 loc gnv fn irf srf tag else ([],[]) in
          (ws' ++ ws, cs' ++ cs, [])
    | VarDec (v, loc, init) ->
        let tag     = CilTag.make_global_t tgr loc in
        let vtyp    = FI.ce_find (FI.name_of_string v.vname) gnv in
        let vspctyp = let vsp, chk = SM.find v.vname varspec in 
                      if chk then vsp else FI.t_true_refctype vtyp in
        let cs'     = cons_of_var_init tag loc gst v vtyp init in
        let cs'',_  = FI.make_cs cf0 FI.ce_empty Ast.pTrue vtyp vspctyp None tag loc in
        let ws'     = FI.make_wfs cf0 FI.ce_empty vtyp tag in
          (ws' ++ ws, cs'' ++ cs' ++ cs, [])
  end (ws, cs, []) decs

let cons_of_scis tgr gnv gst scim shpm ci =
  SM.fold begin fun fn sci ci ->
    let _ = if mydebug then ignore(Pretty.printf "Generating Constraints for %s \n" fn) in 
    cons_of_sci tgr gnv gst sci (SM.find sci.ST.fdec.svar.vname shpm)
    |> Consindex.add ci fn sci
  end scim ci 

let tag_of_global = function
  | GType (_,_)    -> "GType"
  | GCompTag (_,_) -> "GCompTag"
  | _              -> "Global"

let decs_of_file cil = 
  Cil.foldGlobals cil begin fun acc g -> match g with
    | GFun (fdec, loc)                  -> FunDec (fdec.svar.vname, loc) :: acc
    | GVar (v, ii, loc) 
      when not (isFunctionType v.vtype) -> VarDec (v, loc, ii.init) :: acc
    | GVarDecl (v, loc) 
      when not (isFunctionType v.vtype) -> VarDec (v, loc, None) :: acc
    | GVarDecl (v, _)
      when (isFunctionType v.vtype)     -> acc
    | GType _ | GCompTag _
    | GCompTagDecl _| GText _
    | GPragma _                         -> acc
    | _ when !Constants.safe            -> assertf "decs_of_file"
    | _                                 -> E.warn "Ignoring %s: %a \n" (tag_of_global g) d_global g 
                                           |> fun _ -> acc
  end []

let scim_of_file cil =
  cil |> ST.scis_of_file 
      |> List.fold_left begin fun acc sci -> 
           let fn = sci.ST.fdec.svar.vname in
           SM.add fn sci acc
         end SM.empty

let reachable cil scim =
  match !Constants.root with 
  | "" -> 
      (fun _ -> true)
  | f0 when SM.mem f0 scim ->
      (SM.find f0 scim).ST.fdec.svar
      |> CM.reach cil 
      |> List.map (fun v -> (v.vname, ())) 
      >> (List.map fst <+> String.concat "," <+> Printf.printf "Reachable from %s : %s \n" f0) 
      |> Misc.sm_of_list
      |> Misc.flip SM.mem 
  | f0 -> 
      assertf "Unknown root function: %s \n" f0 
        
(*
let print_sccs sccs =
  P.printf "Callgraph sccs:\n\n";
  List.iter (fun fs -> P.printf " [%a]\n" (P.d_list "," (fun () v -> P.text v.Cil.vname)) fs |> ignore) sccs
*)

(************************************************************************************)
(******************************** API ***********************************************)
(************************************************************************************)

(* API *)
let create cil (spec: FI.refspec) =
  let scim     = scim_of_file cil in
  let reachf   = reachable cil scim in
  let scim     = Misc.sm_filter (fun fn _ -> reachf fn) scim in 
  let _        = E.log "\nDONE: SSA conversion \n" in
  let tgr      = scim |> Misc.sm_to_list |> Misc.map snd |> CilTag.create in
  let _        = E.log "\nDONE: TAG initialization\n" in
  let spec     = Misc.app_fst3 (rename_funspec scim) spec in
  let _        = E.log "\nDONE: SPEC rename \n" in
  let shm, cnv = shapem_of_scim cil spec scim in
  let _        = E.log "\nDONE: Shape Inference \n" in
  let _        = if !Constants.ctypes_only then exit 0 else () in
  let decs     = decs_of_file cil |> Misc.filter (function FunDec (vn,_) -> reachf vn | _ -> true) in
  let _        = E.log "\nDONE: Gathering Decs \n" in
  let gnv      = mk_gnv spec cnv decs in
  let _        = E.log "\nDONE: Global Environment \n" in
  let gst      = spec |> FI.RefCTypes.Spec.store |> FI.store_of_refstore |> FI.refstore_fresh "global" in
  (tgr, cons_of_decs tgr spec gnv gst decs
        |> Consindex.create
        |> cons_of_scis tgr gnv gst scim shm)
