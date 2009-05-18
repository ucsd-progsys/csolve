module F  = Format
module ST = Ssa_transform
module  A = Ast
module  P = Ast.Predicate
module  C = Constraint
module Sy = Ast.Symbol
module So = Ast.Sort
module CI = CilInterface

module YM = Sy.SMap
module SM = Misc.StringMap

open Misc.Ops
open Cil

(*******************************************************************)
(**************** Wrapper between LiquidC and Fixpoint *************)
(*******************************************************************)


(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)
type cilname = Sy.t
let cilname_of_varinfo = fun v -> Sy.of_string v.vname
let cilname_of_string = Sy.of_string

type cilreft = Base of C.reft 
             | Fun  of (cilname * cilreft) list * cilreft  

type cilenv  = cilreft YM.t

let ce_empty = 
  YM.empty

let ce_find v (cenv : cilreft YM.t) =
  try YM.find (Sy.of_string v.vname) cenv with Not_found -> 
    assertf "Unknown variable! %s" v.vname 

let ce_add v cr cenv = 
  YM.add (cilname_of_varinfo v) cr cenv

let ce_project base_env fun_env vs =
  vs
  |> Misc.map cilname_of_varinfo 
  |> Misc.filter (fun vn -> not (YM.mem vn base_env))
  |> List.fold_left begin 
       fun env vn -> 
         asserts (YM.mem vn fun_env) "ce_project";
         YM.add vn (YM.find vn fun_env) env
     end base_env

let ce_unroll v env =
  match ce_find v env with
  | Fun (vcrs, cr) -> 
      let env' = List.fold_left 
                   (fun env (vn, cr) -> YM.add vn cr env) 
                   env vcrs in
      (env', cr)
  | _ ->
      assertf "ce_unroll"

let ce_iter f cenv = 
  YM.iter (fun vn cr -> f vn cr) cenv

let env_of_cilenv cenv = 
  YM.fold begin
    fun vn cr env -> 
      match cr with 
      | Base r -> YM.add vn r env
      | _      -> env
  end cenv YM.empty
  
let rec print_cilreft so ppf = function  
  | Base r -> 
      C.print_reft so ppf r 
  | Fun (args, ret) ->
      F.fprintf ppf "(%a) -> %a"
        (Misc.pprint_many false "," (print_binding so)) args
        (print_cilreft so) ret

and print_binding so ppf (vn, cr) = 
  F.fprintf ppf "%a : %a" Sy.print vn (print_cilreft so) cr

let print_ce so ppf cenv =
  YM.iter begin
    fun vn cr -> 
      F.fprintf ppf "@[%a@]@\n" (print_binding so) (vn, cr) 
  end cenv

(*******************************************************************)
(************************** Templates ******************************)
(*******************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let rec cilreft_of_type f = function
  | TInt _  as t -> 
      let ras = f t in
      let vv  = Sy.value_variable So.Int in
      Base (C.make_reft vv So.Int ras )
  | TFun (t, stso, _, _) ->
      Fun (cilreft_of_bindings f stso,
           cilreft_of_type f t)
  | _      -> 
      assertf "TBDNOW: Consgen.fresh"

and cilreft_of_bindings f = function
  | None -> 
      []
  | Some sts -> 
      List.map begin
        fun (s,t,_) -> (cilname_of_string s, cilreft_of_type f t) 
      end sts

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
  Base (C.make_reft vv so ras)

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
      let env' = List.fold_left (fun env (vn, cr) -> YM.add vn cr env) env args in
      (make_wfs env' ret loc) ++ 
      (Misc.flap (fun (_, cr) -> make_wfs env' cr loc) args)

(****************************************************************)
(********************** Constraint Indexing *********************)
(****************************************************************)

type cindex = {
  scim : ST.ssaCfgInfo SM.t;
  wfm  : C.wf list SM.t;
  cm   : C.t list SM.t;
(*  envm : cilenv SM.t; *)
}

(* API *)
let create_t ws cs = 
  { scim = Misc.StringMap.empty;
    wfm  = Misc.StringMap.add Constants.global_name ws Misc.StringMap.empty;
    cm   = Misc.StringMap.add Constants.global_name cs Misc.StringMap.empty;
    envm = SM.empty }

(* API *)
let add_t me fn sci wfs cs env =
  { scim = SM.add fn sci me.scim ;
    wfm  = SM.add fn wfs me.wfm ;
    cm   = SM.add fn cs  me.cm ;
    (* envm = SM.add fn env me.envm *)
  }

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
          F.printf "Liquid Types for %s \n%a" fn (print_ce so) env)

(* API *)
let wfs_of_t = fun me -> SM.fold (fun _ wfs acc -> wfs ++ acc) me.wfm []

(* API *)
let cs_of_t  = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm []

