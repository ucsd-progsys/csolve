module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  C = Constraint
module  W = Wrapper 
module CI = Consinfra
module H  = Hashtbl

open Misc.Ops
open Cil

(* This is stale. Refresh or nuke. *)
let print_cmap ppf cm = 
  SM.iter 
    (fun fn cs -> 
      F.printf "Constraints for %s \n %a" 
        fn (Misc.pprint_many true "\n" (C.print None)) cs)
    cm

let rec reft_of_exp = function _ -> failwith "TBDNOW"

let envt_of_fun (genv : W.cilenv) fdec = 
  List.fold_left 
    (fun g v ->
      let vr = if ST.is_ssa_name v.Cil.vname 
               then W.fresh v.Cil.vtype 
               else reft_of_exp (var_exp v) in
      W.ce_add v vr g)
    genv fdec.Cil.slocals

let wfs_of_block me env i = 
  let envi = W.ce_project env (CI.reach_vars me i) in
  let l    = CI.location me i in
  CI.ssa_targs me i 
  |> Misc.flap (fun v -> W.make_wfs envi (snd (W.ce_find v env)) l)

let cs_of_block me env i = 
  let gi = W.ce_project env ((CI.def_vars me i) ++ (CI.reach_vars me i)) in
  let p  = CI.guardp me i in
  let l  = CI.location me i in
  CI.ssa_srcs me i 
  |> Misc.flap (fun (v,vi) -> W.make_ts gi p (W.t_var vi) (snd (W.ce_find v env)) l)

let cons_of_fun genv sci =
  let me  = CI.create sci in
  let env = envt_of_fun genv sci.ST.fdec in
  (Misc.mapn (wfs_of_block me env) me.size |> Misc.flatten, 
   Misc.mapn (cs_of_block  me env) me.size |> Misc.flatten)

(* NOTE: templates for formals should be in "global" genv *)
(* API *)
let mk_cons genv scis = 
  let xys = Misc.map (cons_of_fun genv) scis in 
  (Misc.flap fst xys, Misc.flap snd xys)
  
(* {{{ 
let phi_cstr cenv doms cfg i (v, bvs) = 
  let rhs = snd (W.ce_find v cenv) in
  let loc = Cil.get_stmtLoc cfg.Ssa.blocks.(i).Ssa.bstmt.skind in
  List.map 
    (fun (j,v') ->
      let lhs = W.t_var v' in 
      W.mk_cilcstr cenv doms.(j) lhs rhs loc)  
    bvs

let gen_phis cenv doms cfg phis : W.cilcstr list = 
  phis
  |> Array.mapi (fun i asgns -> Misc.tr_flap (phi_cstr cenv doms cfg i) asgns) 
  |> Array.to_list 
  |> Misc.tr_flatten


let gen_body fdec doms : A.pred = 
  let fid   = fdec.svar.vname in
  let invsr = ref [] in
  let vis   = new consGenVisitor fid doms invsr in
  let _     = visitCilFunction vis fdec in
  A.pAnd !invsr

 
let gen g sci =
  let fdec = sci.ST.fdec in
  let cfg  = sci.ST.cfg in
  let doma = guards_closure sci.ST.gdoms in
  let phis = sci.ST.phis in
  let g'   = gen_decs g  fdec in
  let invp = gen_body fdec doma in
  let ccs  = gen_phis g' doms cfg phis in
  let cs   = Misc.tr_map (W.cstr_of_cilcstr sci invp) ccs in
  cs

  }}} *)

