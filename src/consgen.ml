module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  C = Constraint
module  W = Wrapper 
module CI = CilInterface

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

(* NOTE: templates for formals should be in "global" g0 *)
let gen_decs (g0 : W.cilenv) fdec = 
  List.fold_left 
    (fun g v ->
      match v.vtype with
      | TInt _ when ST.is_ssa_name v.vname ->
          W.ce_add v (W.fresh v.vtype) g
      | _      -> g)
    g0 fdec.slocals

(***************************************************************************)

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

class consGenVisitor fid doms invsr = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset), e, _) ->
        let p = A.pAtom ((CI.expr_of_var v), A.Eq, (CI.expr_of_cilexp e)) in 
        invsr := p :: !invsr;
        DoChildren 
    | _ -> 
        asserts false "TBD: consGenVisitor vinst";
        assert false

  method vstmt s =
    sid := s.sid;
    DoChildren
end

let gen_body fdec doms : A.pred = 
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
  let doms = guards_closure sci.ST.gdoms in
  let phis = sci.ST.phis in
  let g'   = gen_decs g  fdec in
  let invp = gen_body fdec doms in
  let ccs  = gen_phis g' doms cfg phis in
  let cs   = List.map (W.cstr_of_cilcstr sci invp) ccs in
  (g', cs)

let inst_qual (e: string list) (q: Ast.pred) : Ast.pred list =
  let ms  = ref [] in
  let gms x = match Ast.Expression.unwrap x with
    | Ast.Var x -> if Ast.Symbol.is_wild x then ms := x :: !ms
    | _ -> () in
  let ms  = Ast.Predicate.iter (fun _ -> ()) gms q; !ms in
  let ms  = Misc.sort_and_compact ms in
  let mms = List.rev_map (fun _ -> e) ms in
  let pms = Misc.rev_perms mms in
  let pms = List.rev_map (List.combine ms) pms in
  let sub ys x =
    match Ast.Expression.unwrap x with
      | Ast.Var y ->
         (try Ast.eVar (Ast.Symbol.of_string (List.assoc y ys)) with Not_found -> x)
      | _ -> x in
  List.rev_map (fun ys -> Ast.Predicate.map (fun x -> x) (sub ys) q) pms
  
let inst_quals (g: W.cilenv) (qs: Ast.pred list) = 
  Misc.tr_flap (inst_qual (W.names_of_cilenv g)) qs

let inst (qs: Ast.pred list) (g : W.cilenv) (cs: C.t list) (s: C.soln) : C.soln =
  let ks  = Misc.tr_flap C.get_kvars cs |> List.map snd in
  let qs' = inst_quals g qs in
  List.fold_left (fun s k -> Ast.Symbol.SMap.add k qs' s) s ks 

(* API *)
let mk_cons qs g0 scis = 
  List.fold_left
    (fun (cs, s) sci -> 
      let g, cs' = gen g0 sci in 
      let s'     = inst qs g cs s in
      ((cs' ++ cs), s'))
    ([], Ast.Symbol.SMap.empty) scis

