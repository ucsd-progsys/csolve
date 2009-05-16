module F  = Format
module SM = Misc.StringMap
module ST = Ssa_transform
module  A = Ast
module  P = Ast.Predicate
module  C = Constraint
module Sy = Ast.Symbol
module So = Ast.Sort
module CI = CilInterface

open Misc.Ops
open Cil


(*******************************************************************)
(**************** Wrapper between LiquidC and Fixpoint *************)
(*******************************************************************)


(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)

type cilreft = Base of C.reft 
             | Fun  of (varinfo * cilreft) list * cilreft  

type cilenv  = (Cil.varinfo * cilreft) SM.t

let ce_empty = 
  SM.empty

let ce_find v cenv = 
  try snd (SM.find v.vname cenv)
  with _ -> assertf "Unknown variable! %s" v.vname

let ce_add v cr cenv = 
  SM.add v.vname (v, cr) cenv

let ce_project cenv vs =
  let vm = List.fold_left (fun m v -> SM.add v.vname () m) SM.empty vs in
  Misc.sm_filter (fun vn _ -> SM.mem vn vm) cenv

let ce_iter f cenv = 
  SM.iter (fun _ (v, r) -> f v r) cenv

let env_of_cilenv cenv = 
  SM.fold begin
    fun x (_, cr) env -> 
      match cr with 
      | Base r -> Sy.SMap.add (Sy.of_string x) r env
      | _ -> env
  end cenv Sy.SMap.empty
  
let rec print_cilreft so ppf = function  
  | Base r -> 
      C.print_reft so ppf r 
  | Fun (args, ret) ->
      F.fprintf ppf "(%a) -> %a"
        (Misc.pprint_many false "," (print_binding so)) args
        (print_cilreft so) ret

and print_binding so ppf (v, cr) = 
  F.fprintf ppf "%s:%a" v.vname (print_cilreft so) cr

let print_ce so ppf cenv =
  SM.iter begin
    fun vn (_, cr) -> 
      F.fprintf ppf "@[%s := %a@]\n" vn (print_cilreft so) cr
  end cenv

(*******************************************************************)
(************************** Templates ******************************)
(*******************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let rec cilreft_of_type f t =
  match t with
  | TInt _ -> 
      let ras = f t in
      let vv  = Sy.value_variable So.Int in
      Base (C.make_reft vv So.Int ras )
  | _      -> 
      assertf "TBDNOW: Consgen.fresh"

let is_base = function
  | TInt _ -> true
  | _      -> false

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)

let t_fresh = cilreft_of_type (fun _ -> [C.Kvar ([], fresh_kvar ())]) 
let t_true  = cilreft_of_type (fun _ -> [])

let t_single t e =
  let so  = CI.sort_of_typ t in
  let vv  = Sy.value_variable so in
  let e   = CI.expr_of_cilexp e in
  let ras = [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  C.make_reft vv so ras

let t_var v =
  t_single v.Cil.vtype (Lval ((Var v), NoOffset))

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let rec make_ts env p lhscr rhscr loc =
  match lhscr, rhscr with
  | Base lhsr, Base rhsr -> 
      [C.make_t (env_of_cilenv env) p lhsr rhsr None]
  | _, _ ->
      assertf ("TBD:make_ts")

let rec make_wfs env cr loc =
  match cr with
  | Base r -> 
      [C.make_wf (env_of_cilenv env) r None]
  | Fun (args, ret) ->
      let env'  = List.fold_left (fun env (v,cr) -> ce_add v cr env) env args in
      let retws = make_wfs env' ret loc in
      let argws = Misc.flap (fun (_,cr) -> make_wfs env' cr loc) args in
      retws ++ argws

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
           F.fprintf ppf "Ref-Constraints for %s \n %a" 
           fn (Misc.pprint_many true "\n" (C.print_t None)) cs;
           F.fprintf ppf "WF-Constraints for %s \n %a"
           fn (Misc.pprint_many true "\n" (C.print_wf None)) wfs)
  | Some s -> (* print solution *)
      iter_t me
        (fun fn (_, _, _, env) ->
          F.printf "Liquid Types for %s \n @[%a@]" fn (print_ce so) env)

(* API *)
let wfs_of_t = fun me -> SM.fold (fun _ wfs acc -> wfs ++ acc) me.wfm []

(* API *)
let cs_of_t  = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm []

