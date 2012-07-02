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

module Misc = FixMisc 
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
module RT = Ct.RefCTypes.CType
module RF = Ct.RefCTypes.Field
module RL = Ct.RefCTypes.LDesc
module RS = Ct.RefCTypes.Store
module RCf = Ct.RefCTypes.CFun
module ES = Ct.EffectSet
module ED = EffectDecls
module Cs = Constants
module Sh = Shape
module An = Annots
module T  = CilTag

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
    | Refanno.HInst _
    | Refanno.New _  
    | Refanno.TNew _
    | Refanno.TInst _
    | Refanno.NewC _ -> (gs, is, a::ns)
  end ([], [], []) xs

let lsubs_of_annots ns = 
  Misc.map_partial (function Refanno.New (x,y)    -> Some (x, y)
                   | Refanno.NewC (x,_,y) -> Some (x, y)
                   | Refanno.TNew _ 
                   | Refanno.TInst _
                   | Refanno.HInst _ ->  None
                   | _               -> assertf "cons_of_call: bad ns") ns
    
let tsubs_of_annots ns =
  Misc.map_partial (function Refanno.TNew (x,y) -> Some (x,y)
                   | Refanno.New _ 
                   | Refanno.NewC _
                   | Refanno.TInst _
                   | Refanno.HInst _ ->  None) ns
    
let tinst_of_annots ns = 
  Misc.map_partial (function Refanno.TInst inst -> Some inst
                   | Refanno.New _ 
                   | Refanno.NewC _
                   | Refanno.TNew _ 
                   | Refanno.HInst _ ->  None) ns
  |> M.only_one "More than one TVar instantiation at call site"
    
let hsubs_of_annots env ns =
    List.fold_left begin fun hsub annot ->
      begin match annot with
        | Refanno.HInst s -> s
        | _ -> hsub
      end
    end Ct.StoreSubst.empty ns

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

let cons_of_annot me loc tag grd ffm effs (env, sto, tago) = function 
  | Refanno.Gen  (cloc, aloc) ->
      let _      = CM.assertLoc loc (RS.mem sto cloc) "cons_of_annot: (Gen)!" in
      let sto'   = RS.remove sto cloc in
      let ld1    = (cloc, Ct.refstore_get sto cloc) in
      let ld2    = (aloc, Ct.refstore_get sto aloc) in
      let cds    = FI.make_cs_refldesc env grd ld1 ld2 tago tag loc in
      ((env, sto', tago), cds)

  | Refanno.WGen  (cloc, aloc) ->
      let ld1    = (cloc, Ct.refstore_get sto cloc) in
      let ld2    = (aloc, Ct.refstore_get sto aloc) in
      let sto'   = RS.remove sto cloc in
      ((env, sto', tago), ([], []))

  | Refanno.Ins (ptr, aloc, cloc) ->
      let _          =
        CM.assertLoc loc ((cloc = Sloc.sloc_of_any) || (not (RS.mem sto cloc))) "cons_of_annot: (Ins)!" in
      let strengthen = strengthen_instantiated_aloc ffm ptr aloc in
      let wld',_     = FI.extend_world sto aloc cloc false strengthen loc tag (env, sto, tago) in
      let cs         = aloc
                    |> Ct.refstore_get sto
                    |> RL.indices
                    |> M.negfilter (Index.is_periodic)
                    |> List.map begin function
                         | Index.IInt n ->
                           let eptr = Ct.Ref (aloc, Index.top)
                                   |> FI.t_field_at_block_of ptr n
                                   |> M.flip FI.t_singleton_effect ED.readEffect in
                             FI.make_cs_effect_weaken_type env grd sto eptr (ES.find effs aloc) tago tag loc
                         | _ -> ([], [])
                       end
                    |> M.splitflatten in
      (wld', cs)

  | _ -> assertf "cons_of_annot: New/NewC" 

let cons_of_annots me loc tag grd wld ffm effs annots =
  Misc.mapfold (cons_of_annot me loc tag grd ffm effs) wld annots
  |> Misc.app_snd Misc.splitflatten 

(******************************************************************************)
(************************* Constraints for Expressions ************************)
(******************************************************************************)

let t_exp_with_cs me loc tago tag grd env e =
  let po, rct = FI.t_exp env (CF.ctype_of_expr me e) e in
    match po with
      | None   -> (rct, ([], []))
      | Some p -> (rct, FI.make_cs_assert env grd p tago tag loc)

(****************************************************************************)
(********************** Constraints for Assignments *************************)
(****************************************************************************)

let extend_env me v cr env =
  let ct = CF.ctype_of_varinfo me v in
  let cr = FI.t_ctype_refctype ct cr in
 (* let _  = Pretty.printf "extend_env: %s :: %a \n" v.Cil.vname Ct.d_refctype cr in *)
 FI.ce_adds env [(FA.name_of_varinfo v), cr]

let cons_of_mem me loc tago tag grd env post_mem_env sto effs v eff =
  if !Cs.manual then
    ([], [])
  else
    let rct = FI.t_ptr_footprint env v in
    let l   = rct |> RT.sloc |> M.maybe |> Sloc.canonical in
           FI.make_cs env grd rct (rct |> Ct.ctype_of_refctype |> FI.t_valid_ptr) tago tag loc
       +++ FI.make_cs_effect_weaken_var post_mem_env grd sto v eff (ES.find effs l) tago tag loc

let cons_of_string me loc tag grd (env, sto, tago) e =
  match t_exp_with_cs me loc tago tag grd env e with
    | Ct.Ref (l, _) as rct, cds ->
      let ld2 = RS.find sto l in
      let ld1 = RL.map (RF.map_type FI.t_nullterm_refctype) ld2 in
        (rct, cds +++ FI.make_cs_refldesc env grd (l, ld1) (l, ld2) tago tag loc)
    | _ -> assert false

let is_string_ptr_expr = function
  | CastE (t, (Const (CStr _))) when Cil.isPointerType t -> true
  | Const (CStr _)                                       -> true
  | _                                                    -> false

let var_addr me env v =
  if v.vglob then CF.refctype_of_global me v else FI.ce_find (FA.name_of_varinfo v) env

let cons_of_rval me loc tag grd effs (env, sto, tago) post_mem_env = function
  (* *v *)
  | Lval (Mem e, _) ->
    let v   = CM.referenced_var_of_exp e in
    let cs  = cons_of_mem me loc tago tag grd env post_mem_env sto effs v ED.readEffect in

    let vn  = FA.name_of_varinfo v in
    let fld = Ct.refstore_read loc sto <| FI.ce_find vn env in
    let rct = fld
           |> RF.type_of
           |> M.choose (RF.is_final fld && Ct.is_soft_ptr loc sto <| var_addr me env v)
               (FI.strengthen_type_with_deref (Ast.eVar vn) 0) id
           |> FI.replace_addr v in
      (rct, cs)
  (* x, when x is global *)
  | Lval (Var v, NoOffset) when v.vglob ->
      (CF.refctype_of_global me v, ([], []))
  | AddrOf (Var v, NoOffset) as e when CM.is_fun v ->
      begin match t_exp_with_cs me loc tago tag grd env e with
        | Ct.FRef (f, r) as rct, cds ->
          let f'   = FI.t_fresh_fn f in
          let rct = Ct.FRef (f', r) in
          (rct,cds +++ FI.make_cs_refcfun env grd (FI.ce_find_fn v.vname env) f' tag loc)
        | _ -> assert false
      end
  | e when is_string_ptr_expr e ->
    let rct, cs = cons_of_string me loc tag grd (env, sto, tago) e
    in (rct, cs)
  (* fptr *)
  (* For polymorphism reasons, we may have left a cast in around a fptr *)
  | CastE ((TPtr ((TFun _), _), Lval (Var v, NoOffset)))
  | Lval (Var v, NoOffset) as e when CM.is_funptr v ->
    (v |> FA.name_of_varinfo |> Misc.flip FI.ce_find env), ([], [])
  (* e, where e is pure *)
  | e when CM.is_pure_expr CM.StringsAreNotPure e ->
    t_exp_with_cs me loc tago tag grd env e 
  | e -> 
      E.s <| errorLoc loc "cons_of_rval: impure expr: %a" Cil.d_exp e 

let is_bot_ptr me env v =
  match var_addr me env v with
    | Ct.Ref (_, (Index.IBot, _)) -> true
    | _                           -> false

let is_any_ptr me env v =
  match var_addr me env v with
    | Ct.ARef -> true
    | _       -> false

let cons_of_fptr me loc tag grd effs (env, sto, tago) post_mem_env lval rv =
  let (Ct.FRef (rf, r) as cr),cds1 =
    cons_of_rval me loc tag grd effs (env,sto,tago) post_mem_env rv
  in
  match t_exp_with_cs me loc tago tag grd env lval with
    | Ct.FRef (f, r') as crf,cds2 ->
      let f' = FI.t_fresh_fn f in
      let crf = Ct.FRef (f', r) in 
      let genv = CF.globalenv_of_t me in
      let wfs1, wfs2 = Misc.map_pair (FI.make_wfs_fn genv) (f', rf) in
      let cds3 = FI.make_cs_refcfun env grd rf f' tag loc in
      (crf, cds1+++cds2+++cds3, wfs1++wfs2)
    | _ -> assert false

let cons_of_set me loc tag grd ffm pre_env effs (env, sto, tago) = function
  (* v := &e where e is a function *)
  | ((Var v1, NoOffset) as lval), ((AddrOf (Var v2, NoOffset)) as rv)
       when not v1.Cil.vglob && CM.is_fun v2 ->
      let cr, cds, wfs = cons_of_fptr me loc tag grd effs (pre_env, sto, tago) env (Lval lval) rv in
      (extend_env me v1 cr env, sto, Some tag), cds, wfs
  (* v := e, where v is local *)
  | (Var v, NoOffset), rv when not v.Cil.vglob ->
      let cr, cds = cons_of_rval me loc tag grd effs (pre_env, sto, tago) env rv in
      (extend_env me v cr env, sto, Some tag), cds, []

  (* v := e, where v is global *)
  | (Var v, NoOffset), rv when v.Cil.vglob ->
    E.s <| Cil.errorLoc loc "Trying to write global var %a@!" CM.d_var v

  (* *v := e, where e is pure *)
  | (Mem ev, _), e ->
    let v = CilMisc.referenced_var_of_exp ev in
      if is_bot_ptr me env v then (env, sto, Some tag), ([], []), [] else
      if is_any_ptr me env v then (env, sto, Some tag), ([], []), [] else
      let addr = var_addr me env v in
      let cr', cds1  = cons_of_rval me loc tag grd effs (pre_env, sto, tago) pre_env e in
      let wfs = match cr' with
        | Ct.FRef (f,_) -> FI.make_wfs_fn pre_env f
        | _ -> []
      in
      let cds2       = cons_of_mem me loc tago tag grd pre_env env sto effs v ED.writeEffect in
      let isp  = try Ct.is_soft_ptr loc sto addr with ex ->
                   Errormsg.s <| Cil.errorLoc loc "is_soft_ptr crashes on %s" v.vname in
      if isp then
        let cr   = addr |> Ct.refstore_read loc sto |> RF.type_of |> FI.replace_addr v in
        let cds3 = FI.make_cs env grd cr' cr tago tag loc in
        (env, sto, Some tag), (cds1 +++ cds2 +++ cds3), wfs
      else
        let sto      = cr' |> FI.replace_addr v |> Ct.refstore_write loc sto addr in
        let env, sto = FI.refstore_strengthen_addr loc env sto ffm v.vname addr in
        (env, sto, Some tag), (cds1 +++ cds2), wfs

  | _ -> assertf "TBD: cons_of_set"

let cons_of_set me loc tag grd ffm pre_env effs (env, sto, tago) ((lv, e) as x) =
  Misc.do_catchu (cons_of_set me loc tag grd ffm pre_env effs (env, sto, tago)) x
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
          let ws     = FI.make_wfs env sto frct in
            (extend_env me v frct env, cs, ds, ws)
        else
          (extend_env me v rct env, [], [], [])
  | None              -> (env, [], [], [])
  | _  when !Cs.safe  -> assertf "env_of_retbind"
  | _                 -> (env, [], [], [])

let filter_poly_effects_binds ldebs ns =
  let polys        = Misc.map_partial (function Refanno.NewC (_, _, l) -> Some l | _ -> None) ns in
  let filter binds = M.negfilter (fst <+> M.flip List.mem polys) binds in
    filter ldebs

let instantiate_poly_clocs me env grd loc tag' ((_, st', _) as wld) ns =
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

let rename_binds_slocs subs binds =
  List.map (M.app_fst <| Sloc.Subst.apply subs) binds

let rename_store lsubs subs st =
  st |> FI.refstore_subs_locs lsubs |> FI.refstore_subs FI.t_subs_exps subs

let renamed_store_bindings lsubs subs st =
     st
  |> rename_store lsubs subs
  |> RS.bindings
  |> fun slds -> rename_binds_slocs lsubs slds

let renamed_store_effects_bindings lsubs subs effs st =
  let st   = rename_store lsubs subs st in
  let effs = effs |> FI.effectset_subs_locs lsubs st |> FI.effectset_subs FI.t_subs_exps subs in
     RS.join_effects st effs
  |> fun slds -> rename_binds_slocs lsubs slds

let store_domain_subst_groups lsubs sto =
     sto
  |> RS.domain
  |> List.map (fun sloc -> (sloc, Sloc.Subst.apply lsubs sloc))
  |> M.kgroupby snd

let d_return_opt lsubs subs cr () = function
  | Some (Var v, _) -> P.dprintf "%a :: %a@!" CM.d_var v RT.d_ctype (FI.rename_refctype lsubs subs cr)
  | _               -> P.nil

let call_subst_error loc typestring f (lvo, frt, es) ecrs lsubs subs =
  E.s <| errorLoc loc
    "Call unifies %s locations which are distinct in callee (%a):@!%a@!@!%a%a@!@!%a ::@!  @[%a@]@!@!"
    typestring
    Sloc.Subst.d_subst lsubs
    d_instr (Call (lvo, Lval (Var f, NoOffset), es, loc))
    (d_return_opt lsubs subs <| Ct.ret_of_refcfun frt) lvo
    (P.docList ~sep:P.line <| fun (e, cr) -> P.dprintf "%a :: %a" d_exp e RT.d_ctype cr)
      (List.combine es ecrs)
    CM.d_var f
    RCf.d_cfun frt

let check_inst_slocs_distinct_or_read_only loc f call ecrs lsubs subs sto =
     sto
  |> store_domain_subst_groups lsubs
  |> List.map snd
  |> List.iter begin function
     | [_]   -> ()
     | lsubs ->
       if not <| List.for_all (fst <+> RS.find sto <+> Ct.RefCTypes.LDesc.is_read_only) lsubs then
         call_subst_error loc "non-final" f call ecrs lsubs subs
    end

let check_inst_concrete_slocs_distinct loc f call ecrs lsubs subs sto =
     sto
  |> store_domain_subst_groups lsubs
  |> List.filter (fst <+> Sloc.is_abstract <+> not)
  |> List.map snd
  |> List.iter begin function
     | [_]   -> ()
     | lsubs -> call_subst_error loc "concrete" f call ecrs lsubs subs
     end

let store_bindings_of_store_effects ldbs =
  List.map (fun (l, (b, _)) -> (l, b)) ldbs
    
let slocs_of_bindings lds = 
  List.map fst lds
  |> Misc.sort_and_compact

let d_slocs () ls =
  Pretty.seq ~sep:(Pretty.text ",") ~doit:(Sloc.d_sloc()) ~elements:ls

let d_bindings () x  = x |> slocs_of_bindings |> d_slocs ()
    
let d_ldbind () (l, ld) =
  Pretty.dprintf "[%a |-> %a]" Sloc.d_sloc l Ctypes.RefCTypes.LDesc.d_ldesc ld

let d_cfbind () (l, cf) = 
  Pretty.dprintf "[%a |-> %a]" Sloc.d_sloc l Ctypes.RefCTypes.CFun.d_cfun cf 

let d_ldbinds () llds = 
    Pretty.seq ~sep:(Pretty.text "\n") ~doit:(d_ldbind ()) ~elements:llds

let d_lcfbinds () lcfs = 
    Pretty.seq ~sep:(Pretty.text ",") ~doit:(d_cfbind ()) ~elements:lcfs

let d_bindings () llds = 
  Pretty.dprintf "DATABINDS: @[%a@]" 
    d_ldbinds llds
    
let wfs_of_inst env sto = 
  Misc.flap (snd <+> FI.make_wfs env sto)
    
let apply_inst env tsub sub sto inst frt = match inst with
  | None -> (frt, [])
  | Some i -> 
    let refi = List.map (M.app_snd (Ct.I.CType.subs sub <+> FI.t_fresh)) i in
    (frt |> RCf.inst_tvar tsub refi, wfs_of_inst env sto refi)
      
let insts_of_annots env ns = 
  (lsubs_of_annots ns, 
   hsubs_of_annots env ns, 
   tsubs_of_annots ns,
   tinst_of_annots ns)
    
let lsubs_of_insts (l,_,_,_) = l
    
let instantiate_tvars me env frt (lsubs,_,tsubs,tinst) = 
  let frt', wfs = apply_inst (CF.globalenv_of_t me) tsubs lsubs (fst <| Ct.stores_of_refcfun frt) tinst frt in
  (frt', wfs)
    
let instantiate_store me env grd tag tago loc sto frt (lsubs,hsubs,_,_) = 
  let sto' = RS.map (Ct.ctype_of_refctype <+> FI.t_fresh) sto in
  let wfs = FI.make_wfs_refstore env sto' sto' in
  let cs,_  = FI.make_cs_refstore env grd sto sto' true tago tag loc in
  RCf.subs_store_var hsubs lsubs sto' frt, cs, wfs
    
let sto_of_fref = function
  | Ct.FRef (f,_) -> Some (Ct.stores_of_refcfun f |> snd)
  | _ -> None
    
let sto_of_args cts = 
     M.map_partial sto_of_fref cts
  |> List.fold_left RS.upd RS.empty
      
let instantiate_frefs lsubs cts = 
  cts |>: begin function
    | Ct.FRef (f, r) -> 
      let subs = FI.subs_of_lsubs lsubs f.Ct.sto_out in
      let f = RCf.sub_uqlocs lsubs f 
           |> RCf.map (FI.t_subs_locs lsubs <+> FI.t_subs_names subs) in
          Ct.FRef (f, r)
    | ct -> ct end
    
let cons_of_call me loc i j grd effs pre_mem_env (env, st, tago) f ((lvo, frt, es) as call) ns =
  let tag       = CF.tag_of_instr me i j     loc (T.Raw "cons_of_call: in") in
  let tag'      = CF.tag_of_instr me i (j+1) loc (T.Raw "cons_of_call: out") in
  let insts     = insts_of_annots pre_mem_env ns in
  let lsubs     = lsubs_of_insts insts in
  let (frt', inst_wfs) = instantiate_tvars me pre_mem_env frt insts in
  let args      = frt' |> Ct.args_of_refcfun |> List.map (Misc.app_fst FA.name_of_string) in
  let args, es  = bindings_of_call loc args es in
  let subs      = List.combine (List.map fst args) es in
  let cs0, ecrs = Misc.mapfold begin fun cs e ->
                    let cr, (cs2, _) = cons_of_rval me loc tag grd effs (pre_mem_env, st, tago) pre_mem_env e in
                      (cs ++ cs2, cr)
                  end [] es in
  let ecrs = instantiate_frefs lsubs ecrs in
  let frt',scs,swfs     = instantiate_store me pre_mem_env grd tag tago loc (RS.upd st (sto_of_args ecrs)) frt' insts in
  let args = frt' |> Ct.args_of_refcfun in
  let cs1,_            = FI.make_cs_tuple env grd lsubs subs ecrs (List.map snd args) None tag loc in
  let stbs             = RS.bindings st in
  let call             = (lvo, frt', es) in
  let istbs            = frt'.Ct.sto_in
                      >> check_inst_slocs_distinct_or_read_only loc f call ecrs lsubs subs
                      |> renamed_store_bindings lsubs subs in
  let ostebs           = frt'.Ct.sto_out
                      >> check_inst_concrete_slocs_distinct loc f call ecrs lsubs subs
                      |> renamed_store_effects_bindings lsubs subs frt'.Ct.effects in
  let ostslds          = store_bindings_of_store_effects ostebs in
  let oaslds,ocslds    = List.partition (fst <+> Sloc.is_abstract) ostslds in
  let cs2,_               = FI.make_cs_refstore_binds env grd stbs   istbs true  None tag  loc in
  let cs3,_               = FI.make_cs_refstore_binds env grd oaslds stbs  false None tag' loc in
  (* let ds3                 = [FI.make_dep false (Some tag') None] in  *)

  let st'                 = List.fold_left begin fun st (sloc, ld) ->
                              if RS.mem st sloc then st else RS.add st sloc ld
                            end st ocslds in

  let stebs               = RS.join_effects st' effs in
  let ostebs              = filter_poly_effects_binds ostebs ns in
  let cs4, _              = FI.make_cs_effectset_binds false env grd ostebs stebs tago tag' loc in
  let retctype            = Ct.ret_of_refcfun frt' in
  let env', cs5, ds5, wfs = env_of_retbind me loc grd tag' lsubs subs env st' lvo (Ct.ret_of_refcfun frt') in
  let wld', cs6           = instantiate_poly_clocs me env grd loc tag' (env', st', Some tag') ns in
  wld', (scs ++ cs0 ++ cs1 ++ cs2 ++ cs3 ++ cs4 ++ cs5 ++ cs6, ds5), (swfs ++ wfs ++ inst_wfs) 


let cons_of_ptrcall me loc i j grd effs pre_mem_env ((env, sto, tago) as wld) (lvo, e, es) ns = match e with
  (* v := ( *f )(...), where v is local *)
  | Lval (Var v, NoOffset) when not v.Cil.vglob ->
      begin match v |> FA.name_of_varinfo |> FI.t_name env with
        | Ct.FRef (f, _) ->
          let cs1 = 
            if !Cs.manual then ([], []) else
              let tag   = CF.tag_of_instr me i j loc (T.Raw "cons_of_ptrcall") in
              let rct   = FI.t_fptr_footprint env v        in
              let rct'  = rct |> Ct.ctype_of_refctype |> FI.t_valid_ptr in
              FI.make_cs env grd rct rct' tago tag loc
          in
          let wld, cs2, wfs =
            cons_of_call me loc i j grd effs pre_mem_env (env, sto, tago) v (lvo, f, es) ns
          in (wld, cs1 +++ cs2, wfs)
        | _ -> assert false
      end
  | _ -> assert false

(****************************************************************************)
(********************** Constraints for [instr] *****************************)
(****************************************************************************)

let with_wfs (cs, ds) wfs =
  (cs, ds, wfs)

let cons_of_annotinstr me i grd effs (j, pre_ffm, ((pre_mem_env, _, _) as wld)) (annots, ffm, instr) =
  let gs, is, ns = group_annots annots in
  let loc        = get_instrLoc instr in
  let tagj       = CF.tag_of_instr me i j loc (T.Raw "cons_of_annotinstr") in
  let wld, acds  = cons_of_annots me loc tagj grd wld pre_ffm effs (gs ++ is) in
  match instr with 
  | Set (lv, e, _) ->
      let _        = asserts (ns = []) "cons_of_annotinstr: new-in-set" in
      let wld, cds, wfs = cons_of_set me loc tagj grd ffm pre_mem_env effs wld (lv, e) in
      (j+1, ffm, wld), with_wfs (cds +++ acds) wfs
  | Call (None, Lval (Var fv, NoOffset), _, _) when CilMisc.isVararg fv.Cil.vtype ->
      let _ = Cil.warnLoc loc "Ignoring vararg call" in
      (j+1, ffm, wld), with_wfs acds []
  | Call (lvo, Lval ((Var fv), NoOffset), es, _) ->
      let wld, cds, wfs =
        cons_of_call me loc i j grd effs pre_mem_env wld fv (lvo, FI.ce_find_fn fv.Cil.vname pre_mem_env, es) ns in
      (j+2, ffm, wld), with_wfs (cds +++ acds) wfs
  | Call (lvo, Lval (Mem e, NoOffset), es, _) ->
      let wld, cds, wfs = cons_of_ptrcall me loc i j grd effs pre_mem_env wld (lvo, e, es) ns in
      (j+2, ffm, wld), with_wfs (cds +++ acds) wfs
  | _ -> 
      E.s <| E.error "TBD: cons_of_instr: %a \n" d_instr instr

let cons_of_annotinstr me i grd effs wld (annots, ffm, instr) =
  cons_of_annotinstr me i grd effs wld (annots, ffm, instr)
  >> begin fun _ -> match instr with 
        | Set ((Var v, NoOffset), _, _)
        | Call (Some (Var v, NoOffset), _, _, _) 
        ->  An.annot_asgn v (get_instrLoc instr) (An.AsgnE instr)
        | _ -> ()
     end 

let scalarcons_of_binding me loc tag (j, env) grd j v cr =
  (* let _      = Pretty.printf "scalarcons_of_binding: [v=%s] [cr=%a] \n" v.Cil.vname Ct.d_refctype cr in *)
  let ct   = Ct.ctype_of_refctype cr in
  let cr'    = FI.t_fresh ct in
  let cs, ds = FI.make_cs env grd cr cr' None tag loc in
  (j+1, extend_env me v cr env), (cs, ds, [(v, cr')])

let declared_ptr_type v =
  v.vtype |> ShapeInfra.fresh_heaptype v.vdecl |> FI.t_scalar 

let scalarcons_of_instr me i grd (j, env) instr = 
  let _     = if mydebug then (ignore <| Pretty.printf "scalarcons_of_instr: %a \n" d_instr instr) in
  let loc   = get_instrLoc instr in
  let tag   = CF.tag_of_instr me i j loc (T.Raw "scalarcons_of_instr") in 
  match instr with
  | Set ((Var v, NoOffset), Lval (Var v2, NoOffset), _) 
    when (not v.vglob) && v2.vglob && CM.is_reference v2.vtype ->
         v2
      |> declared_ptr_type
      |> scalarcons_of_binding me loc tag (j, env) grd j v 

  | Set ((Var v, NoOffset), e, _) 
    when (not v.vglob) && CM.is_pure_expr CM.StringsArePure e ->
      FI.t_exp_scalar v e 
      |> scalarcons_of_binding me loc tag (j, env) grd j v 

  | Set ((Var v, NoOffset), e, _) 
    when CM.is_reference v.Cil.vtype && not (CM.is_pure_expr CM.StringsArePure e) ->
         v
      |> declared_ptr_type
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Call (Some (Var v, NoOffset), Lval (Mem _, NoOffset), _, _) ->
      (* Nothing we can do here; we'll have to check this call is ok after we infer
         the contents of memory. *)
         (if CM.is_reference v.Cil.vtype then declared_ptr_type v else FI.t_true Ct.scalar_ctype)
      |> scalarcons_of_binding me loc tag (j, env) grd j v    
     
  | Call (Some (Var v, NoOffset), Lval (Var fv, NoOffset), es, _) ->
      let farg = Ct.args_of_refcfun 
                 <+> List.map (FA.name_of_string <.> fst) 
                 <+> (fun xs -> asserts (List.length xs = List.length es) "mismatched arguments at callsite"; xs)
                 <+> Misc.flip List.combine es in
      let fret = Ct.ret_of_refcfun  <+> FI.t_scalar_refctype in
      env
      |> FI.ce_find_fn fv.Cil.vname
      |> (farg <*> fret)
      |> Misc.uncurry FI.t_subs_exps 
      (* >> E.log "SCALAR CALLASGN: v=%s  cr=%a \n" v.Cil.vname Ct.d_refctype  *)
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Set ((Var v, NoOffset), _, _) 
    when (not v.Cil.vglob)  ->
      v.Cil.vtype |> Ct.vtype_to_ctype |> FI.t_true
      |> scalarcons_of_binding me loc tag (j, env) grd j v

  | Set (_,_,_) | Call (_ , _, _, _) ->
      (j+1, env), ([], [], [])

  | _ -> 
      E.s <| E.error "TBD: scalarcons_of_instr: %a \n" d_instr instr

let scalarcons_of_instr x1 x2 x3 x4 instr = 
  try scalarcons_of_instr x1 x2 x3 x4 instr with ex ->
    halt <| errorLoc (get_instrLoc instr) 
              "scalarcons_of_instr hits exn: %s : %a \n\n"
                (Printexc.to_string ex)        
                d_instr instr

(****************************************************************************)
(********************** Constraints for [stmt] ******************************)
(****************************************************************************)

let cons_of_ret me loc i grd effs (env, st, tago) e_o =
  let tag    = CF.tag_of_instr me i 1000 loc (T.Raw "cons_of_ret") in
  let frt    = FI.ce_find_fn (CF.get_fname me) env in
  let effs, rv_cds =
    match e_o with
      | None   -> (effs, ([], []))
      | Some e -> let lhs, cs = cons_of_rval me loc tag grd effs (env, st, tago) env e in
                  let rhs     = Ct.ret_of_refcfun frt in
                  (effs, cs +++ FI.make_cs env grd lhs rhs tago tag loc) in
  let st_cds = let _, ost = Ct.stores_of_refcfun frt in
               (FI.make_cs_refstore env grd st ost true tago tag loc) in
  (st_cds +++ rv_cds) 

let cons_of_annotstmt me loc i grd effs wld (anns, (ffm, ffms), stmt) =
  match stmt.skind with
  | Instr is ->
      asserts (List.length anns = List.length is) "cons_of_stmt: bad annots instr";
      let (n, _, wld), cdws     =  Misc.combine3 anns ffms is 
                                |> Misc.mapfold (cons_of_annotinstr me i grd effs) (1, ffm, wld) in
      let cs1, ds1, ws          = Misc.splitflatten3 cdws in  
      (wld, cs1, ds1, ws)
  | Return (e_o, loc) ->
      asserts (List.length anns = 0) "cons_of_stmt: bad annots return";
      let cs, ds        = cons_of_ret me loc i grd effs wld e_o in
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

let wcons_of_block_effects me loc sto i =
  if CF.block_has_fresh_effects me i then
    let env = if CM.is_foreach_iter_block <| CF.stmt_of_block me i then
                CF.inenv_of_block me i
              else i |> CF.idom_parblock_of_block me |> CF.inenv_of_block me in
      FI.make_wfs_effectset env sto (CF.effectset_of_block me i)
  else []

let wcons_of_block me loc (_, sto, _) i des =
  let _    = if mydebug then Printf.printf "wcons_of_block: %d \n" i in 
  let csto = if CF.has_shape me then CF.csto_of_block me i else RS.empty in
  let tag  = CF.tag_of_instr me i 0 loc (T.Raw "wcons_of_block") in
  let phis = CF.phis_of_block me i in
  let env  = CF.inenv_of_block me i in
  let wenv = phis |> List.fold_left (weaken_undefined me true) env in
  let ws1  = phis |> List.map  (fun v -> FI.ce_find  (FA.name_of_varinfo v) env)
                  |> Misc.flap (fun cr -> FI.make_wfs wenv sto cr) in
  let ws2  = FI.make_wfs_refstore wenv (RS.upd sto csto) csto in
  let ws3  = des |> Misc.flap (fun (v, cr) -> FI.make_wfs wenv sto cr) in
  let ws4  = wcons_of_block_effects me loc sto i in
  ws1 ++ ws2 ++ ws3 ++ ws4

let cons_of_init_block me loc (env, sto, _) =
  let tag = CF.tag_of_instr me 0 0 loc (T.Raw "cons_of_init_block") in
  FI.make_cs_refstore env Ast.pTrue (FI.conv_refstore_bottom sto) sto true None tag loc

let fresh_effectcons_of_block me loc (env, sto, _) i =
  if CF.block_has_fresh_effects me i then
    let tag           = CF.tag_of_instr me i 0 loc (T.Raw "fresh_effectcons_of_block") in
    let effs          = CF.effectset_of_block me i in
    let grd           = CF.guard_of_block me i None in
    let idompar       = CF.idom_parblock_of_block me i in
    let idomeffs      = CF.effectset_of_block me idompar in
    let _, idomsto, _ = CF.inwld_of_block me idompar in
          FI.make_cs_effectset env grd sto sto (FI.conv_effectset_bottom effs) effs None tag loc
      +++ FI.make_cs_effectset env grd sto idomsto effs idomeffs None tag loc
  else ([], [])

let cobegin_cons_of_block me loc grd env sto b tag loc =
     b
  |> CM.coroutines_of_block
  |> M.pairs
  |> List.map begin fun (j, k) ->
       let effs1, effs2 = M.map_pair (CF.effectset_of_block me) (j, k) in
         FI.make_cs_assert_effectsets_disjoint env grd sto effs1 effs2 None tag loc
     end
  |> M.splitflatten

let foreach_cons_of_block me loc grd i tag loc =
  let idompar     = CF.idom_parblock_of_block me i in
  let idx         = CM.index_var_of_foreach <| CF.stmt_of_block me idompar in
  let nidx        = FA.name_of_varinfo idx in
  let nidx2       = FA.name_fresh () in
  let env, sto, _ = CF.inwld_of_block me i in
  let effs        = CF.effectset_of_block me i in
  let effs2       = FI.effectset_subs (const <| FI.t_subs_names [(nidx, nidx2)]) () effs in
  let env         = FI.ce_adds env [(nidx2, FI.ce_find nidx env)] in
  let grd         = Ast.pAnd [grd; Ast.pAtom (Ast.eVar nidx, Ast.Ne, Ast.eVar nidx2)] in
    FI.make_cs_assert_effectsets_disjoint env grd sto effs effs2 None tag loc

let effect_disjoint_cons_of_block me loc grd (env, sto, _) i =
  let b   = CF.stmt_of_block me i in
  let tag = CF.tag_of_instr me i 0 loc (T.Raw "effect_disjoint_cons_of_block") in
    if CM.is_cobegin_block b then
      cobegin_cons_of_block me loc grd env sto b tag loc
    else if CM.is_foreach_iter_block b then
      foreach_cons_of_block me loc grd i tag loc
    else ([], [])

let cons_of_block me i =
  let _                = if mydebug then Printf.printf "cons_of_block: %d \n" i in 
  let loc              = CF.location_of_block me i in
  let grd              = CF.guard_of_block me i None in
  let astmt            = CF.annotstmt_of_block me i in
  let wld              = CF.inwld_of_block me i in
  let ws1              = wcons_of_block me loc wld i [] in
  let cs3, _           = fresh_effectcons_of_block me loc wld i in
  let cs4, _           = effect_disjoint_cons_of_block me loc grd wld i in
  let effs             = CF.effectset_of_block me i in
  let wld, cs, ds, ws2 = cons_of_annotstmt me loc i grd effs wld astmt in
  let cs2, ds2         = if i = 0 then cons_of_init_block me loc wld else ([], []) in
  (wld, (ws1 ++ ws2, cs ++ cs2 ++ cs3 ++ cs4, [], ds ++ ds2))

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

let var_cons_of_edge me envi loci tagi grdij envj subs vjvis =
  Misc.flap_pair begin fun (vj, vi) ->
    let envi = weaken_undefined me false envi vj          in
    let lhs  = let ni = FA.name_of_varinfo vi             in
               if (CF.is_undefined me vi) then 
                 FI.ce_find ni envi 
                 |> Ct.ctype_of_refctype 
                 |> FI.t_true
               else FI.t_name envi ni                     in 
    let rhs  = let nj = FA.name_of_varinfo vj             in
               FI.ce_find nj envj |> FI.t_subs_names subs in
    let cs = FI.make_cs envi grdij lhs rhs None tagi loci in 
    let _  = An.annot_asgn vj loci (An.AsgnV vi)          in
    cs
  end vjvis

let gen_cons_of_edge me iwld' loci tagi grdij i j =
  CF.annots_of_edge me i j
  |> cons_of_annots me loci tagi grdij iwld' Sloc.SlocMap.empty ES.empty
  |> snd

let join_cons_of_edge me (envi, isto', _) loci tagi grdij subs i j =
  let rsto   = CF.csto_of_block me j |> FI.refstore_subs FI.t_subs_names subs  in 
  let lsto,_ = Ct.refstore_partition (fun cl -> RS.mem rsto cl) isto' in
  FI.make_cs_refstore envi grdij lsto rsto true None tagi loci

let cons_of_edge me i j =
  let _     = if mydebug then Printf.printf "cons_of_edge: %d --> %d \n" i j in 
  let iwld' = CF.outwld_of_block me i in
  let loci  = CF.location_of_block me i in
  let tagi  = CF.tag_of_instr me i 0 loci (T.Raw "cons_of_edge") in
  let grdij = CF.guard_of_block me i (Some j) in (* >> (Ast.Predicate.to_string <+> E.log "guard_of_edge (%d -> %d) = %s \n" i j) *)
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
  let tagi  = CF.tag_of_instr me i 0 loci (T.Raw "scalarcons_of_edge") in
  let grdij = CF.guard_of_block me i (Some j) in
  let envj  = CF.outwld_of_block me j |> fst3 in
  let vjvis = CF.asgns_of_edge me i j in
  let subs  = List.map (Misc.map_pair FA.name_of_varinfo) vjvis in
  (var_cons_of_edge me (fst3 iwld') loci tagi grdij envj subs vjvis)

let cons_of_edge me = if CF.has_shape me then cons_of_edge me else scalarcons_of_edge me

(****************************************************************************)
(********************** Constraints for Ssa_transform.t *********************)
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

let should_check_loc_type sts l = match Sloc.SlocMap.find l sts with
  | Ct.IsSubtype             -> true
  | Ct.HasShape | Ct.HasType -> false

let cons_of_global_store tgr spec decs gst =
  let tag       = T.make_global_t tgr Cil.locUnknown (T.Raw "cons_of_global_store") in
  let ws        = FI.make_wfs_refstore FI.ce_empty gst gst in
  let sts       = CS.locspectypes spec in
  let check_sto = spec |> CS.store |> RS.partition (should_check_loc_type sts) |> fst in
  let cs, _     = FI.make_cs_refstore FI.ce_empty Ast.pTrue gst check_sto true None tag Cil.locUnknown in
    (ws, cs)

let add_offset loc t ctptr off =
  match ctptr with
  | Ct.Ref (s, (i, r)) ->
      Ct.Ref (s, (off |> CilMisc.bytesOffset t |> Index.of_int |> Index.plus i, r))
  | _ -> halt <| errorLoc loc "Adding offset to bogus type: %a\n\n" Ct.d_refctype ctptr

let rec cons_of_init (sto, cs) tag loc env cloc t ctptr = function
  | SingleInit e ->
      let cr     = ctptr |> Ct.refstore_read loc sto |> RF.type_of in
      let ct     = Ct.ctype_of_refctype cr in
      let _, cr' = FI.t_exp env ct e in
        if Ct.is_soft_ptr loc sto ctptr then
          let env = FI.ce_adds env [(FI.vv_addr, cloc |> Sloc.canonical |> FI.t_addr)] in
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

let cons_of_var_init tag loc sto v vtyp = function
  | Some (CompoundInit _ as init) ->
      let t_var       = v.vtype |> CilMisc.bytesSizeOf |> FI.t_size_ptr (Ct.ctype_of_refctype vtyp) in
      let cs1, _      = FI.make_cs FI.ce_empty Ast.pTrue t_var vtyp None tag loc in
      let aloc, r     = match vtyp with Ct.Ref (al, r) -> (al, r) | _ -> assert false in
      let cloc        = Sloc.copy_concrete aloc in
      let ctptr       = Ct.Ref (cloc, r) in
      let env, sto, _ = fst <| FI.extend_world sto aloc cloc true id loc tag (FI.ce_empty, sto, None) in
      let sto, cs2    = cons_of_init (sto, []) tag loc env cloc v.vtype ctptr init in
      let ld1         = (cloc, Ct.refstore_get sto cloc) in
      let ld2         = (aloc, Ct.refstore_get sto aloc) in
      let cs3, _      = FI.make_cs_refldesc env Ast.pTrue ld1 ld2 None tag loc in
        cs1 ++ cs2 ++ cs3
  | Some (SingleInit e) ->
      let _, t_var = FI.t_exp FI.ce_empty (Ct.ctype_of_refctype vtyp) e in
        fst <| FI.make_cs FI.ce_empty Ast.pTrue t_var vtyp None tag loc
  | None -> assert (v.vstorage = Extern); []

let init_of_var v = function
  | None -> if v.vstorage = Extern then None else Some (makeZeroInit v.vtype)
  | i    -> i

(******************************************************************************)
(*************************** Interpreting Spectypes ***************************)
(******************************************************************************)

let should_subtype = function
  | Ct.IsSubtype | Ct.HasType -> true
  | Ct.HasShape               -> false

let should_supertype = function
  | Ct.HasType                 -> true
  | Ct.IsSubtype | Ct.HasShape -> false

let make_cs_if b lcs =
  if b then lcs |> Lazy.force |> fst else []

(*************************************************************************)
(****************************** API **************************************)
(*************************************************************************)

(* API *)
let cons_of_decs tgr spec gnv gst decs =
  let ws, cs = cons_of_global_store tgr spec decs gst in
  List.fold_left begin fun (ws, cs, _, _) -> function
    | CM.FunDec (fn, fd, loc) ->
        let tag      = T.make_t tgr loc fn 0 0 (T.Spec ("cons_of_dec fun", fd.svar)) in
        let irf      = FI.ce_find_fn fn gnv in
        let ws'      = FI.make_wfs_fn gnv irf in
        let srf, s   = spec |> CS.funspec |> SM.find fn in
        let cs'      = make_cs_if (should_subtype s)
                         (lazy (FI.make_cs_refcfun gnv Ast.pTrue irf srf tag loc)) in
        let cs''     = make_cs_if (should_supertype s)
                         (lazy (FI.make_cs_refcfun gnv Ast.pTrue srf irf tag loc)) in
        (ws' ++ ws, cs'' ++ cs' ++ cs, [], [])
    | CM.VarDec (v, loc, init) ->
        let tag        = T.make_global_t tgr loc (T.Spec ("cons_of_dec var", v)) in
        let vtyp       = FI.ce_find (FA.name_of_string v.vname) gnv in
        let vspctyp, s = spec |> CS.varspec |> SM.find v.vname in 
        let cs'        = cons_of_var_init tag loc gst v vtyp (init_of_var v init) in
        let cs''       = make_cs_if (should_subtype s)
                           (lazy (FI.make_cs FI.ce_empty Ast.pTrue vspctyp vtyp None tag loc)) in
        let ws'        = FI.make_wfs FI.ce_empty gst vtyp in
        let cs'''      = make_cs_if (should_supertype s)
                           (lazy (FI.make_cs FI.ce_empty Ast.pTrue vtyp vspctyp None tag loc)) in
          (ws' ++ ws, cs''' ++ cs'' ++ cs' ++ cs, [], [])
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
