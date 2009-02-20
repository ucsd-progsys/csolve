module CG = Consgen
module ST = Ssa_transform
module  E = Ast.Expression
module  P = Ast.Predicate
module  C = Ast.Constraint
module  T = Ast.Template
module SM = Misc.StringMap
module IM = Misc.IntMap
module TP = TheoremProver

open Misc.Ops

(**************** converting constraints to predicates ******************)

let mk_tpltps t s = 
  match T.get_refn t with 
  | T.Conc p        -> [p]
  | T.Kvar (k,subs) -> List.map (P.substitute subs) (T.s_lookup s k)

let mk_envp g s =
  T.p_env (T.s_apply_env s g)

let mk_bkgrdp sci g invs c s = 
  let gps   = C.expand_guard sci.ST.ifs c in
  let lhsps = mk_tpltps (C.get_lhs c) s in
  let envp  = (mk_envp g s) in
  Misc.tr_flatten [[envp]; invs; gps; lhsps]

(************************** refinement functions ***********************)

let refine_constr tp cm sci g invs c s = 
  match T.get_refn (C.get_rhs c) with 
  | T.Conc _ -> 
      failwith "refine_constr: rhs is concrete"
  | T.Kvar (k,subs) -> 
      let bgps = mk_bkgrdp sci g invs c s in
      let qs   = IM.find k s in
      let qqs  = List.map (fun p -> (p, P.substitute subs p)) qs in
      let qqs' = tp#set_and_filter bgps qqs in
      let qs'  = List.map fst qqs' in
      let s'   = IM.add k qs' s in
      (s', (List.length qs) != (List.length qs')) 

let refine_fun tp cm fn sci g invs cs s =
  Misc.fixpoint 
    (fun s ->
      List.fold_left 
        (fun (s,b) c -> 
          let (s',b') = refine_constr tp cm sci g invs c s in
          (s', (b' || b)))
        (s, false) cs) 
    s

let refine_pgm tp cm s =
  Misc.fixpoint
    (fun s ->
      SM.fold 
        (fun fn (sci, g, invs, cs) (s,b) -> 
          let (s',b') = refine_fun tp cm fn sci g invs cs s in
          (s', (b' || b)))
        cm (s, false)) 
    s

(********************* top-level solving functions ***********************)

let initial (qs: P.t list) (cm: Consgen.t) : T.soln = 
  let ks = Consgen.get_kvars cm in
  List.fold_left (fun s k -> T.s_update s k qs) IM.empty ks

let solve (env: TP.sort SM.t) (cm: CG.t) (qs: P.t list) : T.soln = 
  let s    = initial qs cm in
  let tp   = new TP.prover env in
  let s',_ = refine_pgm tp cm s in
  s'
