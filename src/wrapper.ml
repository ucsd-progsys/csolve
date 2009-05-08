module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  C = Constraint
module Sy = A.Symbol
module So = A.Sort
module CI = CilInterface

open Misc.Ops
open Cil


(*******************************************************************)
(**************** Wrapper between LiquidC and Fixpoint *************)
(*******************************************************************)

type cilenv  = (Cil.varinfo * C.reft) SM.t
type cilcstr = cilenv * (int * bool) list * C.reft * C.reft * Cil.location

let ce_empty = SM.empty
let ce_find  = fun v cenv -> SM.find v.vname cenv
let ce_add   = fun v r cenv -> SM.add v.vname (v, r) cenv

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let fresh ty : C.reft =
  match ty with
  | TInt _ ->
      C.make_reft (Sy.value_variable So.Int) So.Int [C.Kvar ([], fresh_kvar ())]
  | _      -> 
      assertf "TBD: Consgen.fresh"

(* manipulating cilenvs *)

let names_of_cilenv c =
  SM.fold (fun x _ xs -> x :: xs ) c []

(* creating refinements *)

let t_single (e : Cil.exp) : C.reft =
  let ty = Cil.typeOf e in
  let so = CI.sort_of_typ ty in
  let vv = Sy.value_variable so in
  let p  = A.pAtom (A.eVar vv, A.Eq, CI.expr_of_cilexp e) in
  (vv, so, [(C.Conc p)])

let t_var (v : Cil.varinfo) : C.reft =
  t_single (Lval ((Var v), NoOffset))

let expand_guard ifs ibs =
  ibs  
  |> List.map (fun (i,b) -> match ifs.(i) with 
               | Some (e,_,_) -> 
                   let p  = CI.pred_of_cilexp e in
                   if b then p else (A.pNot p)
               | _ -> 
                   assertf "ERROR: expand_guard")
  |> A.pAnd

let mk_cilcstr cenv ibs lhst rhst loc = 
  (cenv, ibs, lhst, rhst, loc)

let env_of_cilenv cenv = 
  SM.fold 
    (fun x (_,r) env -> Sy.SMap.add (Sy.of_string x) r env) 
    cenv
    Sy.SMap.empty
(*
let cstr_of_cilcstr sci p (cenv, ibs, r1, r2, _) =
  failwith "TBDNOW"
  let env = env_of_cilenv cenv in
  let gp  = expand_guard  sci.ST.ifs ibs in
  C.make_t env (A.pAnd [p; gp]) r1 r2 
  *)

let cstr_of_cilcstr _ = failwith "TBDNOW"

