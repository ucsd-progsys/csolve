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

open Misc.Ops
open Cil

let mydebug = true 

(****************************************************************************)
(***************************** Misc. Helpers ********************************)
(****************************************************************************)

let group_annots xs = 
  List.fold_left begin fun (gs, is, ns) a -> 
    match a with 
    | Refanno.Gen _ -> (a::gs, is, ns)
    | Refanno.Ins _ -> (gs, a::is, ns)
    | Refanno.New _ -> (gs, is, a::ns)
  end ([], [], []) xs

let lsubs_of_annots ns = 
  List.map (function Refanno.New (x,y) -> (x,y) 
                     | _               -> assertf "cons_of_call: bad ns") ns

let extend_world ld binds loc (env, sto) = 
  let subs   = List.map (fun (n,_) -> (n, FI.name_fresh ())) binds in
  let env'   = List.map2 (fun (_, cr) (_, n') -> (n', cr)) binds subs
               |> Misc.map (Misc.app_snd (FI.t_subs_names subs))
               |> FI.ce_adds env in
  let _, im  = List.fold_left (fun (i,im) (_,n') -> (i+1, IM.add i n' im)) (0,IM.empty) subs in
  let sto'   = FI.refldesc_subs ld (fun i _ -> IM.find i im |> FI.t_name env') 
               |> FI.refstore_set sto loc in
  (env', sto')

let extend_env v cr env =
  FI.ce_adds env [(FI.name_of_varinfo v), cr]

let rename_store lsubs subs sto = 
  sto |> Ctypes.prestore_subs lsubs 
      |> FI.refstore_subs_exps subs 

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
      let pj     = CF.guard_of_block me j in
      let locj   = CF.location_of_block me j in
      let envj,_ = CF.outwld_of_block me j in
      let nnjs   = Misc.map (Misc.map_pair FI.name_of_varinfo) vvjs in
      Misc.flap begin fun (v, vj) ->
        if CF.is_undefined me vj then [] else  
          let envj  = weaken_undefined me envj v in
          let n, nj = Misc.map_pair FI.name_of_varinfo (v, vj) in
          let lhs   = FI.t_name envj nj in
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

let cons_of_annot loc grd ((env, sto) as wld) = function 
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
      let wld    = extend_world aldesc abinds cloc (env, sto) in
      (wld, [])

  | Refanno.New (aloc, cloc) ->
      let _      = assertf "cons_of_annot: New!" in
      let _      = asserts (not (FI.refstore_mem cloc sto)) "cons_of_annot: (New)!" in
      (wld, [])

let cons_of_annots me loc grd wld annots =
  Misc.mapfold (cons_of_annot loc grd) wld annots
  |> Misc.app_snd Misc.flatten 

(****************************************************************************)
(********************** Constraints for Assignments *************************)
(****************************************************************************)

let cons_of_set me (env, sto) = function 
  (* v := *v' *)
  | (Var v, NoOffset), Lval (Mem (Lval (Var v', offset)), _) 
  | (Var v, NoOffset), Lval (Mem (CastE (_, Lval (Var v', offset))), _) ->
      let _  = asserts (offset = NoOffset) "cons_of_set: bad offset1" in
      let cr = FI.ce_find (FI.name_of_varinfo v') env 
               |> FI.refstore_read sto 
               |> FI.t_ctype_refctype (CF.ctype_of_varinfo me v) in
      (extend_env v cr env, sto)

  (* v := e, where e is pure *)
  | (Var v, NoOffset), e ->
      let _  = CilMisc.check_pure_expr e in
      let cr = FI.t_exp (CF.ctype_of_expr me e) e  
               |> FI.t_ctype_refctype (CF.ctype_of_varinfo me v) in
      (extend_env v cr env, sto)
  
  (* *v := e, where e is pure *)
  | (Mem (Lval(Var v, NoOffset)), _), e 
  | (Mem (CastE (_, Lval (Var v, _))), _), e ->
      let addr = FI.ce_find (FI.name_of_varinfo v) env in
      let sto' = FI.t_exp (CF.ctype_of_expr me e) e
                 |> FI.refstore_write sto addr in
      (env, sto')
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

let cons_of_call me loc grd (env, st) (lvo, fn, es) ns = 
  let frt   = FI.ce_find_fn fn env in
  let args  = FI.args_of_refcfun frt in
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
  ((env', st'), cs1 ++ cs2 ++ cs3)
  
  (* {{{ OLD CODE FOR CONS_OF_CALL
  let (ncrs, cr) = FI.ce_find_fn fn env in
  let _    = asserts (List.length ncrs = List.length es) "cons_of_call: length" in
  let ns   = Misc.map fst ncrs in
  let crs  = List.map (fun e -> FI.t_exp (CF.ctype_of_expr me e) e) es in
  let cenv = FI.ce_adds env (List.combine ns crs) in
  let cs   = Misc.flap begin fun (n, cr) -> 
                FI.make_cs cenv grd (FI.t_name cenv n) cr loc
             end ncrs in
  match lvo with 
  | None -> ((env, cst), cs)  
  | Some ((Var v), NoOffset) ->
      let vn  = FI.name_of_varinfo v in
      let cr' = cr |> FI.t_subs_exps (List.combine ns es) in
      ((FI.ce_adds env [vn, cr'], cst), cs)
  | _  -> assertf "TBD: cons_of_call" 
 *)
 (*  OLD CODE TO HANDLE MALLOC
 if fv.Cil.vname = "malloc" && !Constants.dropcalls then 
   let _          = asserts (is = []) "cons_of_annotinstr: ins-in-call" in
   let wld, cs    = cons_of_annots me loc grd wld gs in
   match ns, lvo with
   | [Refanno.New (aloc, cloc)], Some ((Var v), NoOffset) ->
       (* step 1: add bindings for new cells *)
       let aldesc     = FI.refstore_get (CF.get_astore me) aloc in
       let abinds     = FI.binds_of_refldesc aloc aldesc 
                        |> List.map (Misc.app_snd FI.t_true_refctype) in
       let (env, sto) = extend_world aldesc abinds cloc wld in 
       (* step 2: add bindings for returned ptr *)
       let cr         = CF.ctype_of_varinfo me v |> FI.t_true in
       let env        = FI.ce_adds env [FI.name_of_varinfo v, cr] in
       (env, sto), []
   | _ -> assertf "cons_of_annotinstr: malformed malloc call!" 
  else 
    END HACK }}} *)

(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let cons_of_annotinstr me loc grd wld (annots, instr) = 
  let gs, is, ns = group_annots annots in
  let wld, anncs = cons_of_annots me loc grd wld (gs ++ is) in
  match instr with 
  | Set (lv, e, _) ->
      let _       = asserts (ns = []) "cons_of_annotinstr: new-in-set" in
      let wld     = cons_of_set me wld (lv, e) in
      (wld, anncs)
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
  let grd      = CF.guard_of_block me i in
  let loc      = CF.location_of_block me i in
  let phis     = CF.phis_of_block me i in
  let astmt    = CF.annotstmt_of_block me i in
  let env, cst = CF.inwld_of_block me i in
  let env      = List.map (bind_of_phi me) phis |> FI.ce_adds env in
  let ws       = wcons_of_phis me loc env phis in
  let wld, cs  = cons_of_annotstmt me loc grd (env, cst) astmt in
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

let cons_of_sci gnv sci =
  let (locals, ctm, store) = Inferctypes.infer_sci_shapes sci in
  let (anna, theta) = Refanno.annotate_cfg sci.ST.cfg ctm in
  let _ = Pretty.printf "%a\n" Refanno.d_block_annotation_array anna in
  let _ = Pretty.printf "%a\n" Refanno.d_ctab theta in 
  CF.create gnv sci (locals, ctm, store) (anna, theta)
  |> Misc.foldn process_block (Array.length sci.ST.phis)
  |> process_phis sci.ST.phis
  |> CF.get_cons

(***************************************************************************)
(*************** Processing SCIs *******************************************)
(***************************************************************************)

let add_scis gnv scis ci = 
  List.fold_left begin 
    fun ci sci ->
      let fn = sci.ST.fdec.Cil.svar.Cil.vname in
      cons_of_sci gnv sci
      |> Misc.uncurry (Consindex.add ci fn sci)
  end ci scis

let scis_of_file cil = 
  Cil.foldGlobals cil begin
    fun acc g ->
      match g with 
      | Cil.GFun (fdec,loc) -> 
          let sci = ST.fdec_to_ssa_cfg fdec loc in
          sci::acc
      | _ -> acc
  end []
  |> (fun scis -> let _ = if mydebug then ST.print_scis scis in scis)


(************************************************************************************)
(***************** Processing Globals ***********************************************)
(************************************************************************************)

let type_of_fdec fdec = 
  let fn = fdec.svar.vname in
  match fdec.svar.vtype with 
  | TFun (a,Some xts, b, c) ->
      let xts' = List.map (fun (x,d,e) -> (x^"@"^fn, d, e)) xts in
      TFun (a, Some xts', b, c)
  | TFun (_,_,_,_) as t -> 
      t
  | _  -> 
      assertf "type_of_fdec"

(* NOTE: 1. templates for formals are in "global" gnv, 
         2. each function var is bound to its "output" *) 
let gnv_of_file cil spec =                       
  Cil.foldGlobals cil begin
    fun gnv g ->
      match g with
      | GFun (fdec, _) ->
          let fn = fdec.svar.vname in
          let ft = Misc.StringMap.find fn spec 
                   |> FI.cfun_of_refcfun
                   |> FI.t_fresh_fn in 
          FI.ce_adds_fn gnv [(fn, ft)] 
      | _ ->
          if !Constants.safe then assertf "gnv_of_file" else
            let _ = ignore (E.warn "Ignoring global: %a \n" d_global g) in 
            gnv
  end FI.ce_empty 

let cons_of_globals gnv cil = 
  Cil.foldGlobals cil begin
    fun (ws, cs) g ->
      match g with
      | Cil.GFun (fdec, loc) ->
          let fn = FI.name_of_varinfo fdec.svar in
          let cr = FI.ce_find fn gnv in
          ((FI.make_wfs gnv cr loc) ++ ws, cs)
      | _ -> 
          E.warn "Ignoring global %a \n" d_global g;
          (ws, cs)

  end ([], []) 

(************************************************************************************)
(******************************** API ***********************************************)
(************************************************************************************)

(* API *)
let create cil spec =
  let _   = failwith "TBDNOW: Consgen.create hookup with infer_shapes" in
  let gnv = gnv_of_file cil spec in
  cons_of_globals gnv cil 
  |> Misc.uncurry Consindex.create
  |> add_scis gnv (scis_of_file cil)
