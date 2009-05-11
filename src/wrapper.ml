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


(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)

type cilenv  = (Cil.varinfo * C.reft) SM.t

let ce_empty = 
  SM.empty

let ce_find v cenv = 
  SM.find v.vname cenv

let ce_add v r cenv = 
  SM.add v.vname (v, r) cenv

let ce_project cenv vs = 
  List.fold_left 
    (fun cenv' v -> SM.add v.vname (ce_find v cenv) cenv')
    ce_empty vs

let ce_iter f cenv = 
  SM.iter (fun _ (v, r) -> f v r) cenv


let env_of_cilenv cenv = 
  SM.fold 
    (fun x (_,r) env -> Sy.SMap.add (Sy.of_string x) r env) 
    cenv
    Sy.SMap.empty

(*******************************************************************)
(************************** Templates ******************************)
(*******************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let fresh ty : C.reft =
  match ty with
  | TInt _ ->
      C.make_reft (Sy.value_variable So.Int) So.Int [C.Kvar ([], fresh_kvar ())]
  | _      -> 
      assertf "TBD: Consgen.fresh"

(*****************************************************************)
(************************** Refinements **************************)
(*****************************************************************)

let t_single t e =
  let so = CI.sort_of_typ t in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp e in
  C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))]

let t_var v =
  t_single v.Cil.vtype (Lval ((Var v), NoOffset))

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let make_ts env p lhsr rhsr loc = 
  [C.make_t (env_of_cilenv env) p lhsr rhsr None]

let make_wfs env r loc =
  let env = env_of_cilenv env in
  C.kvars_of_reft r 
  |> List.map (fun (_,k) -> C.make_wf env k None)

(****************************************************************)
(********************** Constraint Indexing *********************)
(****************************************************************)

type t = {
  scim : ST.ssaCfgInfo SM.t;
  wfm  : C.wf list SM.t;
  cm   : C.t list SM.t;
  envm : cilenv SM.t;
}

(* API *)
let empty_t = 
  { scim = SM.empty;
    wfm  = SM.empty;
    cm   = SM.empty;
    envm = SM.empty }

(* API *)
let add_t me fn sci wfs cs env =
  { scim = SM.add fn sci me.scim ;
    wfm  = SM.add fn wfs me.wfm ;
    cm   = SM.add fn cs  me.cm ;
    envm = SM.add fn env me.envm }

let find_t me fn = 
  (SM.find fn me.scim, 
   SM.find fn me.wfm,
   SM.find fn me.cm,
   SM.find fn me.envm)

let iter_t me f = 
  SM.iter (fun fn _ -> f fn (find_t me fn)) me.scim

(* API *)
let print_t so ppf me = 
  match so with 
  | None -> (* print constraints *) 
      iter_t me 
        (fun fn (_, wfs, cs, _) ->
           F.printf "Ref-Constraints for %s \n %a" 
           fn (Misc.pprint_many true "\n" (C.print_t None)) cs;
           F.printf "WF-Constraints for %s \n %a"
           fn (Misc.pprint_many true "\n" (C.print_wf None)) wfs)
  | Some s -> (* print solution *)
      iter_t me
        (fun fn (_, _, _, (env: cilenv)) ->
          F.printf "Liquid Types for %s \n %a" fn
          (C.print_env so) (env_of_cilenv env))

(* API *)
let wfs_of_t = fun me -> SM.fold (fun _ wfs acc -> wfs ++ acc) me.wfm []

(* API *)
let cs_of_t  = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm []

