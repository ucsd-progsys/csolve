module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  E = Ast.Expression
module  P = Ast.Predicate
module  C = Constraint
module  W = Wrapper 

open Cil



(*******************************************************************)
(*******************************************************************)
(*******************************************************************)

let print_cmap ppf (cm:t) = 
  SM.iter 
    (fun fn cs -> 
      F.printf "Constraints for %s \n %a" 
        fn (Misc.pprint_many true "\n" C.print) cs)
    cm

(* NOTE: templates for formals should be in "global" g0 *)
let gen_decs (g0 : W.cilenv) fdec = 
  List.fold_left 
    (fun g v ->
      match v.vtype with
      | TInt _ when ST.is_ssa_name v.vname ->
          W.cilenv_add v (W.fresh v.vtype) g
      | _      -> g)
    g0 fdec.slocals

(***************************************************************************)

let phi_cstr cenv doms cfg i (v, bvs) : W.cilcstr = 
  let rhs = W.ce_find cenv v in
  let loc = Cil.get_stmtLoc cfg.Ssa.blocks.(i).Ssa.bstmt.skind in
  List.map 
    (fun (j,v') ->
      let lhs = W.t_var v' in 
      W.mk_cilcstr cenv doms.(j) lhs rhs loc)  
    bvs

let gen_phis env doms cfg phis : W.cilcstr list = 
  phis
  |> Array.mapi (fun i asgns -> Misc.tr_flap (phi_cstr cenv doms cfg i) asgns) 
  |> Array.to_list 
  |> Misc.tr_flatten

class consGenVisitor fid doms invsr = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset) as lv , e, _) ->
        let p = A.pAtom ((W.expr_of_lval lv), A.Eq, (W.expr_of_cilexpr e)) in 
        invsr := p :: !invsr;
        DoChildren 
    | _ -> 
        asserts false "TBD: consGenVisitor vinst" 

  method vstmt s =
    sid := s.sid;
    DoChildren
end

let gen_body fdec doms : A.pred list = 
  let fid   = fdec.svar.vname in
  let invsr = ref [] in
  let vis   = new consGenVisitor fid doms invsr in
  let _     = visitCilFunction vis fdec in
  A.pAnd !invsr

let guards_closure gdoms = 
  let n    = Array.length gdoms in
  let doms = Array.make n [] in 
  for i = 0 to n - 1 do
    match gdoms.(i) with
    | (j, Some b) when 0 <= j && j < i -> Array.set doms i ((j,b)::doms.(j))
    | (j, None  ) when 0 <= j && j < i -> Array.set doms i (doms.(j))
    | _                                -> ()
  done;
  doms
 
let gen g sci =
  let fdec = sci.ST.fdec in
  let cfg  = sci.ST.cfg in
  let fid  = fdec.svar.vname in
  let doms = guards_closure sci.ST.gdoms in
  let phis = sci.ST.phis in
  let g'   = gen_decs g  fdec in
  let invp = gen_body fdec doms in
  let ccs  = gen_phis g' doms cfg phis in
  let cs   = List.map (W.cstr_of_cilcstr sci invp) ccs in
  (g', cs)

(* API *)
let mk_cons g0 scis = 
  List.fold_left 
    (fun m sci -> 
      let k       = sci.ST.fdec.Cil.svar.Cil.vname in
      let (g, cs) = gen g0 sci in
      SM.add k (sci, g, cs) m)
    SM.empty scis
