module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  C = Constraint
module  W = Wrapper 
module CI = CilInterface
module H  = Hashtbl

open Misc.Ops
open Cil

(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

let print_cmap ppf cm = 
  SM.iter 
    (fun fn cs -> 
      F.printf "Constraints for %s \n %a" 
        fn (Misc.pprint_many true "\n" (C.print None)) cs)
    cm

(***************************************************************************)
(************* Infrastructure **********************************************)
(***************************************************************************)

type t = {
  sci      : ST.ssaCfgInfo;
  size     : int;                                    (* number of blocks *)
  reacha   : Cil.varinfo list array;                 (* block |-> vars reaching block-start *)
  defa     : Cil.varinfo list array;                 (* block |-> vars defined inside block *)
  guarda   : (int * bool) list array;                (* block |-> (dom guard * bool) list *)
  ssa_srca : (Cil.varinfo * Cil.varinfo) list array; (* block |-> (x, xi) list, 
                                                                  st. xi defined in block,
                                                                      x = phi(...xi...) *)
  ssa_targa : Cil.varinfo list array;                (* block |-> ssa-vars defined in block *)
  vart     : (string, Cil.exp) H.t;                  (* var |-> defininng assignment *)
}

(* FIX *)
class consGenVisitor fid doms var_exprt = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset), e, _) ->
        H.add var_exprt v.Cil.vname e;
        DoChildren 
    | _ -> 
        asserts false "TBD: consGenVisitor vinst";
        assert false

  method vstmt s =
    sid := s.sid;
    DoChildren
end

(* FIX *)
let guards_closure gdoma = 
  let n    = Array.length gdoma in
  let doma = Array.make n [] in 
  for i = 0 to n - 1 do
    match gdoma.(i) with
    | (j, Some b) when 0 <= j && j < i -> Array.set doma i ((j,b)::doma.(j))
    | (j, None  ) when 0 <= j && j < i -> Array.set doma i (doma.(j))
    | _                                -> ()
  done;
  doma

let expand_guard ifs ibs =
  ibs  
  |> List.map (fun (i,b) -> match ifs.(i) with 
               | Some (e,_,_) -> 
                   let p  = CI.pred_of_cilexp e in
                   if b then p else (A.pNot p)
               | _ -> 
                   assertf "ERROR: expand_guard")
  |> A.pAnd


(* API *)
let location = fun me i -> Cil.get_stmtLoc me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt.skind
let ssa_srcs = fun me i -> Misc.getf me.ssa_srca i "Consgen.ssa_srcs"
let ssa_targs= fun me i -> Misc.getf me.ssa_targa i "Consgen.ssa_srcs"
let var_expr = fun me v -> failwith "TBDNOW"
let reach    = fun me i -> Misc.getf me.reacha i "Consgen.reach"
let defs     = fun me i -> Misc.getf me.defa i "Consgen.defs" 
let guardp   = fun me i -> (Misc.getf me.guarda i "Consgen.guardp") 
                           |> expand_guard me.sci.ST.ifs 

(* API *)
let create sci = failwith "TBDNOW"
(*
  let size = Array.length sci.ST.gdoms in
let vart = H.create 17 in
  {
  size   = size;
  reacha = Array.init size (fun i -> ...);
  defa   = Array.init size (fun i -> ...);
  guarda = guards_closure sci.ST.gdoms;
  vart  : (string, Cil.varinfo * int * Cil.exp) H.t  (* var |-> varinfo, defblock, defexpr *) 
*)

let rec reft_of_expr = function _ -> failwith "TBDNOW"

let envt_of_fun (genv : W.cilenv) fdec = 
  List.fold_left 
    (fun g v ->
      let vr = if ST.is_ssa_name v.Cil.vname 
               then W.fresh v.Cil.vtype 
               else reft_of_expr (var_expr v) in
      W.ce_add v vr g)
    genv fdec.Cil.slocals

let wfs_of_block me env i = 
  let envi = W.ce_project env (reach me i) in
  let l    = location me i in
  ssa_targs me i 
  |> Misc.flap (fun v -> W.make_wfs envi (snd (W.ce_find v env)) l)

let cs_of_block me env i = 
  let gi = W.ce_project env ((defs me i) ++ (reach me i)) in
  let p  = guardp me i in
  let l  = location me i in
  ssa_srcs me i 
  |> Misc.flap (fun (v,vi) -> W.make_ts gi p (W.t_var vi) (snd (W.ce_find v env)) l)

let cons_of_fun genv sci =
  let me  = create sci in
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

