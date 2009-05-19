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
type name = Sy.t
let name_of_varinfo = fun v -> Sy.of_string v.vname
let name_of_string = Sy.of_string

type cilreft = Base of C.reft 
             | Fun  of (name * cilreft) list * cilreft  

type cilenv  = cilreft YM.t

let ce_empty = 
  YM.empty

let ce_mem n cenv = 
  YM.mem n cenv

let ce_find n (cenv : cilreft YM.t) =
  try YM.find n cenv with Not_found -> 
    assertf "Unknown name! %s" (Sy.to_string n)

let ce_add n cr cenv = 
  YM.add n cr cenv

let ce_project base_env fun_env ns =
  ns
  |> Misc.filter (fun vn -> not (YM.mem vn base_env))
  |> List.fold_left begin 
       fun env n -> 
         asserts (YM.mem n fun_env) "ce_project";
         YM.add n (YM.find n fun_env) env
     end base_env

let ce_unroll n env =
  match ce_find n env with
  | Fun (vcrs, cr) -> 
      let env' = List.fold_left 
                   (fun env (n', cr') -> YM.add n' cr' env) 
                   env vcrs in
      (env', cr)
  | _ ->
      assertf "ce_unroll"

let ce_iter f cenv = 
  YM.iter (fun n cr -> f n cr) cenv

let env_of_cilenv cenv = 
  YM.fold begin
    fun n cr env -> 
      match cr with 
      | Base r -> YM.add n r env
      | _      -> env
  end cenv YM.empty
       
let rec print_cilreft so ppf = function  
  | Base r -> 
      C.print_reft so ppf r 
  | Fun (args, ret) ->
      F.fprintf ppf "(%a) -> %a"
        (Misc.pprint_many false "," (print_binding so)) args
        (print_cilreft so) ret

and print_binding so ppf (n, cr) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_cilreft so) cr

let print_ce so ppf cenv =
  YM.iter begin
    fun n cr -> 
      F.fprintf ppf "@[%a@]@\n" (print_binding so) (n, cr) 
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
        fun (s,t,_) -> (name_of_string s, cilreft_of_type f t) 
      end sts

let is_base = function
  | TInt _ -> true
  | _      -> false

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)

let t_fresh = cilreft_of_type (fun _ -> [C.Kvar ([], fresh_kvar ())]) 
let t_true  = cilreft_of_type (fun _ -> [])

let t_exp env e =
  let so  = CI.sort_of_typ (Cil.typeOf e) in
  let vv  = Sy.value_variable so in
  let e   = CI.expr_of_cilexp e in
  let ras = [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  Base (C.make_reft vv so ras)

(*
let t_var env v =
  asserts (ce_mem v env) "t_var: reading unbound var"; 
  t_exp env v.Cil.vtype (Lval ((Var v), NoOffset))
*)

let t_name env n = 
  asserts (YM.mem n env) "t_cilname: reading unbound var";
  match YM.find n env with
  | Base r -> 
      let so  = C.sort_of_reft r in
      let vv  = Sy.value_variable so in
      let ras = [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
      Base (C.make_reft vv so ras)
  | cr -> cr

let t_subs cr nes = failwith "TBDNOW: t_subs"

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let rec make_cs env p lhscr rhscr loc =
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
