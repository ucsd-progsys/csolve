module ST = Ssa_transform
module  E = Ast.Expression
module  P = Ast.Predicate
module  C = Constraint
module Mm = Metamucil
module SM = Misc.StringMap

open Cil

type t = (ST.ssaCfgInfo * Mm.cilenv * P.t list * C.t list) SM.t

let print_tplt () (u, r) = 
  Pretty.dprintf "{V: %a | %a}" d_type u C.print_refinement r 

let get_kvars (cm: t) = 
  let ks = 
    SM.fold
      (fun _ (_,_,_,cs) ks -> 
        let ks' = Misc.tr_flap C.get_kvars cs in
        List.rev_append ks ks')
      cm [] in
  Misc.sort_and_compact ks

let print_cmap (cm:t) = 
  SM.iter
    (fun fn (sci, g, invs, cs) -> 
      ignore(Pretty.printf "Templates for %s \n" fn);
      SM.iter (fun vn (t, r, _) -> ignore(Pretty.printf "%s |-> %a \n" vn
        print_tplt (t, r))) g;
      ignore(Pretty.printf "Invariants for %s \n" fn);
      List.iter (fun p -> ignore(Pretty.printf "%s \n" (P.to_string p))) invs;
      ignore(Pretty.printf "Constraints for %s \n" fn);
      List.iter (fun c -> ignore(Pretty.printf "%a \n" 
        (C.print None std_formatter) c)) cs)
    cm

(***************************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> incr r; C.Kvar ([], string_of_int !r)

let rec fresh ty =
  match ty with
  | TInt _ -> (ty, (fresh_kvar ()))  
  | _      -> failwith "Unhandled: fresh"

let gen_decs g0 fdec = 
  let gen_var g v =
    match v.vtype with
    | TInt _ when ST.is_ssa_name v.vname -> SM.add v.vname ((fresh v.vtype), v) g
    | _      -> g in
  (* templates for formals should be in "global" g
   * let g1 = List.fold_left gen_var g0 fdec.sformals in *)
  let g2 = List.fold_left gen_var g0 fdec.slocals  in
  g2

(******************************************************************************)

let phi_loc cfg i =
  get_stmtLoc cfg.Ssa.blocks.(i).Ssa.bstmt.skind

let gen_phis g fid doms cfg phis = 
  let phi_cstr i (v,bvs) = 
    let vt  = Mm.ciltyp_of_var g v in
    (*let loc = phi_loc cfg i in*)
    List.map
      (fun (j,v') ->
        Mm.mk_constr envTBA (*fid*) (Mm.expand_guard doms.(j) phis)
          (Mm.mk_const_reft (Ast.eVar v')) (??? vt) (*loc*)) bvs in
  (* TODO: find or build cilenv, figure out RHS, grab and port expand_guard *)
  let _, cs = 
    Array.fold_left 
      (fun (i,cs) asgns -> 
        let cs' = Misc.tr_flap (phi_cstr i) asgns in
        let cs' = List.rev_append cs cs' in
        (i+1, cs'))
      (0, []) phis in
  cs

(******************************************************************************)

class consGenVisitor fid doms invsr = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset) as lv , e, l) ->
        invsr := (Mm.mk_eq (Lval lv) e) :: !invsr;
        DoChildren 
    | _ -> 
        failwith "Unhandled: consGenVisitor vinst" 

  method vstmt s =
    sid := s.sid;
    DoChildren
end

let gen_body fdec doms = 
  let fid   = fdec.svar.vname in
  let invsr = ref [] in
  let vis   = new consGenVisitor fid doms invsr in
  let _     = visitCilFunction vis fdec in
  !invsr

(******************************************************************************)

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
  let invs = gen_body fdec doms in
  let cs   = gen_phis g' fid doms cfg phis in
  (g', invs, cs)

(* API *)
let mk_cons g0 scis = 
  List.fold_left 
    (fun m sci -> 
      let k             = sci.ST.fdec.Cil.svar.Cil.vname in
      let (g, invs, cs) = gen g0 sci in
      SM.add k (sci, g, invs, cs) m)
    SM.empty scis


